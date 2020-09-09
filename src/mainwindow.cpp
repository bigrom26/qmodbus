/*
 * mainwindow.cpp - implementation of MainWindow class
 *
 * Copyright (c) 2009-2013 Tobias Junghans / Electronic Design Chemnitz
 *
 * This file is part of QModBus - http://qmodbus.sourceforge.net
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program (see COPYING); if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA.
 *
 */

#include <stddef.h>
#include <stdlib.h>

#include <QWidget>
#include <QSettings>
#include <QDebug>
#include <QScrollBar>
#include <QClipboard>
#include <QCheckBox>
#include <QMessageBox>
#include <errno.h>
#include <QDateTime>
#include <QTextBrowser>
#include <QTextDocument>

#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "BatchProcessor.h"
#include "modbus.h"
#include "modbus-private.h"


const int DataTypeColumn    = 0;
const int AddrColumn        = 1;
const int DataColumnHex     = 2;
const int DataColumnDec     = 3;

extern MainWindow * globalMainWin;


MainWindow::MainWindow( QWidget * _parent ) :
	QMainWindow( _parent ),
	ui( new Ui::MainWindowClass ),
	m_modbus( NULL ),
	m_tcpActive(false),
    m_poll(false),
    LastTick(DWORD(-1)),
    DataNoByte(0)
{
	ui->setupUi(this);

	connect( ui->rtuSettingsWidget,   SIGNAL(serialPortActive(bool)), this, SLOT(onRtuPortActive(bool)));
	connect( ui->asciiSettingsWidget, SIGNAL(serialPortActive(bool)), this, SLOT(onAsciiPortActive(bool)));
	connect( ui->tcpSettingsWidget,   SIGNAL(tcpPortActive(bool)),    this, SLOT(onTcpPortActive(bool)));

	connect( ui->rtuSettingsWidget,   SIGNAL(connectionError(const QString&)), this, SLOT(setStatusError(const QString&)));
	connect( ui->asciiSettingsWidget, SIGNAL(connectionError(const QString&)), this, SLOT(setStatusError(const QString&)));
	connect( ui->tcpSettingsWidget,   SIGNAL(connectionError(const QString&)), this, SLOT(setStatusError(const QString&)));

	connect( ui->slaveID, SIGNAL( valueChanged( int ) ),
			this, SLOT( updateRequestPreview() ) );
	connect( ui->functionCode, SIGNAL( currentIndexChanged( int ) ),
			this, SLOT( updateRequestPreview() ) );
	connect( ui->startAddr, SIGNAL( valueChanged( int ) ),
			this, SLOT( updateRequestPreview() ) );
	connect( ui->numCoils, SIGNAL( valueChanged( int ) ),
			this, SLOT( updateRequestPreview() ) );

	connect( ui->functionCode, SIGNAL( currentIndexChanged( int ) ),
			this, SLOT( updateRegisterView() ) );
	connect( ui->numCoils, SIGNAL( valueChanged( int ) ),
			this, SLOT( updateRegisterView() ) );
	connect( ui->startAddr, SIGNAL( valueChanged( int ) ),
			this, SLOT( updateRegisterView() ) );

	connect( ui->sendBtn, SIGNAL( clicked() ),
			this, SLOT( onSendButtonPress() ) );

	connect( ui->clearBusMonTable, SIGNAL( clicked() )	, this, SLOT( clearBusMonTable() ) );

	connect( ui->actionAbout_QModBus, SIGNAL( triggered() )	, this, SLOT( aboutQModBus() ) );

//	connect( ui->pbMacroHelp        , SIGNAL( clicked() )                 , this, SLOT( showMacroHelp() ) );
	connect( ui->functionCode	, SIGNAL( currentIndexChanged( int ) ),
		this, SLOT( enableHexView() ) );

	connect( ui->checkBoxHexAddress, SIGNAL(toggled(bool))                , this, SLOT( checkBoxHexAddress_toggle(bool) ) );

	updateRegisterView();
	updateRequestPreview();
	enableHexView();

	m_statusInd = new QWidget;
	m_statusInd->setFixedSize( 16, 16 );
	m_statusText = new QLabel;
	ui->statusBar->addWidget( m_statusInd );
	ui->statusBar->addWidget( m_statusText, 10 );
	resetStatus();

	QTimer * t = new QTimer( this );
	connect( t, SIGNAL(timeout()), this, SLOT(pollForDataOnBus()));
	t->start( 5 );

	m_pollTimer = new QTimer( this );
	connect( m_pollTimer, SIGNAL(timeout()), this, SLOT(sendModbusRequest()));

	m_statusTimer = new QTimer( this );
	connect( m_statusTimer, SIGNAL(timeout()), this, SLOT(resetStatus()));
	m_statusTimer->setSingleShot(true);
	ui->tabWidget->setCurrentIndex(0);

	ui->tabWidget->setCurrentIndex(0);

	ui->regTable->setColumnWidth( DataTypeColumn, 250 );
	ui->regTable->setColumnWidth( AddrColumn    ,  70 );
	ui->regTable->setColumnWidth( DataColumnHex ,  70 );
	ui->regTable->setColumnWidth( DataColumnDec ,  70 );

	ui->busMonTable->setColumnWidth( 0, 25 ); // IO
	ui->busMonTable->setColumnWidth( 1, 25 ); // ID
	ui->busMonTable->setColumnWidth( 2, 30 ); // Fn Code
	ui->busMonTable->setColumnWidth( 3, 30 ); // Start Address
	ui->busMonTable->setColumnWidth( 4, 30 ); // Num of Coils
	ui->busMonTable->setColumnWidth( 5, 25 ); // CRC

//	ui->dwMacro->setVisible( ui->actionMacros->isChecked() );

}


MainWindow::~MainWindow()
{
	delete ui;
}

void MainWindow::keyPressEvent(QKeyEvent* event)
{
	if( event->key() == Qt::Key_Control )
	{
		//set flag to request polling
		if( m_modbus != NULL )
			m_poll = true;

		if( ! m_pollTimer->isActive() )
			ui->sendBtn->setText( tr("Poll") );
	}
}

void MainWindow::keyReleaseEvent(QKeyEvent* event)
{
	if( event->key() == Qt::Key_Control )
	{
		m_poll = false;

		if( ! m_pollTimer->isActive() )
			ui->sendBtn->setText( tr("Send") );
	}
}

void MainWindow::onSendButtonPress( void )
{
	//if already polling then stop
	if( m_pollTimer->isActive() )
	{
		m_pollTimer->stop();
		ui->sendBtn->setText( tr("Send") );
	}
	else
	{
		//if polling requested then enable timer
		if( m_poll )
		{
			m_pollTimer->start( 1000 );
			ui->sendBtn->setText( tr("Stop") );
		}

		sendModbusRequest();
	}
}

void MainWindow::busMonitorAddItem( bool isRequest,
					uint8_t slave,
					uint8_t func,
					uint16_t addr,
					uint16_t nb,
					uint16_t expectedCRC,
					uint16_t actualCRC )
{
	QTableWidget * bm = ui->busMonTable;
	while( bm->rowCount() >= 1000 ) {
		bm->removeRow(0);
	}

	const int rowCount = bm->rowCount();
	bm->setRowCount( rowCount+1 );

	QTableWidgetItem * ioItem = new QTableWidgetItem( isRequest ? tr( "Req >>" ) : tr( "<< Resp" ) );
	QTableWidgetItem * slaveItem = new QTableWidgetItem( QString::number( slave ) );
	QTableWidgetItem * funcItem = new QTableWidgetItem( QString::number( func ) );
	QTableWidgetItem * addrItem = new QTableWidgetItem( QString::number( addr ) );
	QTableWidgetItem * numItem = new QTableWidgetItem( QString::number( nb ) );
	QTableWidgetItem * crcItem = new QTableWidgetItem;
	if( func > 127 )
	{
		addrItem->setText( QString() );
		numItem->setText( QString() );
		funcItem->setText( tr( "Exception (%1)" ).arg( func-128 ) );
		funcItem->setForeground( Qt::red );
	}
	else
	{
		if( expectedCRC == actualCRC )
		{
			crcItem->setText( QString().sprintf( "%04X", actualCRC ) );
		}
		else
		{
			crcItem->setText( QString().sprintf( "%04X (%04X)", actualCRC, expectedCRC ) );
			crcItem->setForeground( Qt::red );
		}
	}
	ioItem->setFlags( ioItem->flags() & ~Qt::ItemIsEditable );
	slaveItem->setFlags( slaveItem->flags() & ~Qt::ItemIsEditable );
	funcItem->setFlags( funcItem->flags() & ~Qt::ItemIsEditable );
	addrItem->setFlags( addrItem->flags() & ~Qt::ItemIsEditable );
	numItem->setFlags( numItem->flags() & ~Qt::ItemIsEditable );
	crcItem->setFlags( crcItem->flags() & ~Qt::ItemIsEditable );
	bm->setItem( rowCount, 0, ioItem );
	bm->setItem( rowCount, 1, slaveItem );
	bm->setItem( rowCount, 2, funcItem );
	bm->setItem( rowCount, 3, addrItem );
	bm->setItem( rowCount, 4, numItem );
	bm->setItem( rowCount, 5, crcItem );
	bm->verticalScrollBar()->setValue( 100000 );
}

void MainWindow::DataRawAddHtml( const QString &textHtml )
{
    QTextCursor cursor      =ui->rawData->textCursor();
    //QTextDocument *document =ui->rawData->document();
    cursor.insertHtml( textHtml );
    ui->rawData->moveCursor(QTextCursor::End);
//    qDebug("html=[%s]", textHtml.toStdString().c_str() );
}

static const char *spanAqua     = "<span style=\"background-color:#B4FFFF;\">";
//    static const char *spanAqua1    = "<span style=\"background-color:#74FFFF;\">";
//    static const char *spanFiolet   = "<span style=\"background-color:#B4B4FF;\">";
static const char *spanFiolet2  = "<span style=\"background-color:#EFFFFF;\">";
//    static const char *spanFiolet1  = "<span style=\"background-color:#9696FF;\">";
static const char *spanYellow   = "<span style=\"background-color:#FFFF55;\">";
    static const char *spanGreen     = "<span style=\"background-color:#55FF55;\">";
    static const char *spanGreen1    = "<span style=\"background-color:#C5FFC5;\">";
//    static const char *spanGray     = "<span style=\"background-color:#AAAAAA;\">";
static const char *spanRed     = "<span style=\"background-color:red;color:white;\">";

static const char *spanEnd      = "</span>";

void MainWindow::busMonitorRawDataSend( uint8_t * data, uint8_t dataLen, bool addNewline )
{
    ui->rawData->moveCursor(QTextCursor::End);
    DataRawAddHtml(">>");
    for( int i = 0; i < dataLen; ++i )
    {
        QString txt;
        txt.sprintf("%s&nbsp;%02X%s", i % 2 ? spanGreen : spanGreen1,data[i], spanEnd );
        DataRawAddHtml(txt);
    }
    if( addNewline )
    {
        DataRawAddHtml("<br>");
    }
    ui->rawData->verticalScrollBar()->setValue( 100000 );
    ui->rawData->setLineWrapMode( QTextBrowser::NoWrap );

    LastTick = GetTickCount();
}

void MainWindow::busMonitorRawData( uint8_t * data, uint8_t dataLen, bool addNewline )
{
#if 1
    if( dataLen > 0 )
	{
        ui->rawData->moveCursor(QTextCursor::End);
        //ui->rawData->insertPlainText("");
        DWORD tm = GetTickCount();
        if ( LastTick != DWORD(-1) ) {
            DWORD dtm = tm - LastTick;
            if ( dtm ) { // skip 0ms
                //ui->rawData->setStyleSheet("background-color:#FFFF55;"); // Yellow
                QString txt;
                txt.sprintf( "%s&nbsp;%ums%s", spanYellow, dtm, spanEnd );
                //ui->rawData->insertHtml(txt);
                DataRawAddHtml( txt );
            }

        }
        LastTick =tm;

        for( int i = 0; i < dataLen; ++i )
        {
            QString txt;
            txt.sprintf("%s&nbsp;%02X%s", (++DataNoByte % 2) ? spanAqua : spanFiolet2,data[i], spanEnd );
            DataRawAddHtml(txt);
        }
        if( addNewline )
        {
            DataRawAddHtml("<br>");
        }
        ui->rawData->verticalScrollBar()->setValue( 100000 );
        ui->rawData->setLineWrapMode( QTextBrowser::NoWrap );
//        qDebug("html=[%s]", ui->rawData->toHtml().toStdString().c_str() );
	}
#else
	if( dataLen > 0 )
	{
		QString dump = ui->rawData->toPlainText();
		for( int i = 0; i < dataLen; ++i )
		{
			dump += QString().sprintf( "%.2x ", data[i] );
		}
		if( addNewline )
		{
			dump += "\n";
		}
		ui->rawData->setPlainText( dump );
		ui->rawData->verticalScrollBar()->setValue( 100000 );
		ui->rawData->setLineWrapMode( QPlainTextEdit::NoWrap );
	}
#endif
}

// static
void MainWindow::stBusMonitorAddItem( modbus_t * modbus, uint8_t isRequest, uint8_t slave, uint8_t func, uint16_t addr, uint16_t nb, uint16_t expectedCRC, uint16_t actualCRC )
{
    Q_UNUSED(modbus);
#if 1
    if ( isRequest ) {
        globalMainWin->LastTick = GetTickCount();
        uint8_t data[10];
        data[0] = slave;
        data[1] = func;
        data[2] = addr / 256;
        data[3] = addr & 255;
        data[4] = nb / 256;
        data[5] = nb & 255;
#if 0
        data[6] = actualCRC / 256;
        data[7] = actualCRC & 255;
#else
        data[6] = expectedCRC / 256;
        data[7] = expectedCRC & 255;
#endif

        globalMainWin->busMonitorRawDataSend( data, 8, true );
        globalMainWin->DataNoByte =0;
    }
#endif
    globalMainWin->busMonitorAddItem( isRequest, slave, func, addr, nb, expectedCRC, actualCRC );
}

// static
void MainWindow::stBusMonitorRawData( modbus_t * modbus, uint8_t * data, uint8_t dataLen, uint8_t addNewline )
{
    Q_UNUSED(modbus);
    globalMainWin->busMonitorRawData( data, dataLen, addNewline != 0 );
}

static QString descriptiveDataTypeName( int funcCode )
{
	switch( funcCode )
	{
		case MODBUS_FC_READ_COILS:
		case MODBUS_FC_WRITE_SINGLE_COIL:
		case MODBUS_FC_WRITE_MULTIPLE_COILS:
			return "Coil (binary)";
		case MODBUS_FC_READ_DISCRETE_INPUTS:
			return "Discrete Input (binary)";
		case MODBUS_FC_READ_HOLDING_REGISTERS:
		case MODBUS_FC_WRITE_SINGLE_REGISTER:
		case MODBUS_FC_WRITE_MULTIPLE_REGISTERS:
			return "Holding Register (16 bit)";
		case MODBUS_FC_READ_INPUT_REGISTERS:
			return "Input Register (16 bit)";
		default:
			break;
	}
	return "Unknown";
}


static inline QString embracedString( const QString & s )
{
	return s.section( '(', 1 ).section( ')', 0, 0 );
}


static inline int stringToHex( QString s )
{
//	return s.replace( "0x", "" ).toInt( NULL, 16 );
    return strtol( s.toStdString().c_str(), NULL, 0);
}


void MainWindow::clearBusMonTable( void )
{
	ui->busMonTable->setRowCount( 0 );
}


void MainWindow::updateRequestPreview( void )
{
    const int slave = ui->slaveID ->value();
    const int func  = stringToHex( embracedString( ui->functionCode->currentText() ) );
    const int addr  = ui->startAddr->value();
    const int num   = ui->numCoils ->value();
	if( func == MODBUS_FC_WRITE_SINGLE_COIL || func == MODBUS_FC_WRITE_SINGLE_REGISTER )
	{
		ui->requestPreview->setText(
            QString().sprintf( "0x%02X, 0x%02X, 0x%04X, ",
                    slave, func, addr ) );
	}
	else
	{
		ui->requestPreview->setText(
            QString().sprintf( "0x%02X, 0x%02X, 0x%04X, 0x%04X, ",
                    slave, func, addr, num  ) );
	}
}




void MainWindow::updateRegisterView( void )
{
	const int func = stringToHex( embracedString(
					ui->functionCode->currentText() ) );
	const QString dataType = descriptiveDataTypeName( func );
	const int addr = ui->startAddr->value();

	int rowCount = 0;
    bool is16Bit = true;
	switch( func )
	{
		case MODBUS_FC_WRITE_SINGLE_REGISTER:
		case MODBUS_FC_WRITE_SINGLE_COIL:
			ui->numCoils->setEnabled( false );
			rowCount = 1;
			break;
		case MODBUS_FC_WRITE_MULTIPLE_COILS:
		case MODBUS_FC_WRITE_MULTIPLE_REGISTERS:
			rowCount = ui->numCoils->value();
		default:
			ui->numCoils->setEnabled( true );
			break;
	}

	ui->regTable->setRowCount( rowCount );

	for( int i = 0; i < rowCount; ++i )
	{
#if 1
		regTable_add( i, is16Bit, addr, dataType, -1 );
#else
		QTableWidgetItem * dtItem = new QTableWidgetItem( dataType );
		QTableWidgetItem * addrItem =
			new QTableWidgetItem( QString::number( addr+i ) );
		QTableWidgetItem * dataItem =
			new QTableWidgetItem( QString::number( 0 ) );
		dtItem->setFlags( dtItem->flags() & ~Qt::ItemIsEditable	);
		addrItem->setFlags( addrItem->flags() & ~Qt::ItemIsEditable );
		ui->regTable->setItem( i, DataTypeColumn, dtItem );
		ui->regTable->setItem( i, AddrColumn, addrItem );
		ui->regTable->setItem( i, DataColumn, dataItem );
#endif
	}

	//ui->regTable->setColumnWidth( 0, 150 );
}


void MainWindow::enableHexView( void )
{
	const int func = stringToHex( embracedString(
					ui->functionCode->currentText() ) );

	bool b_enabled =
		func == MODBUS_FC_READ_HOLDING_REGISTERS ||
		func == MODBUS_FC_READ_INPUT_REGISTERS;

//	ui->checkBoxHexData->setEnabled( b_enabled );
}

void MainWindow::regTable_add( int i, bool is16Bit, uint16_t addr,
                               QString dataType, int32_t data )
{
    QString qs_num;
//    bool b_hex = is16Bit && ui->checkBoxHexData->checkState() == Qt::Checked;

    QTableWidgetItem *dtItem    =new QTableWidgetItem( dataType );
    dtItem  ->setFlags( dtItem  ->flags() & ~Qt::ItemIsEditable );
    ui->regTable->setItem( i, DataTypeColumn, dtItem   );

    bool b_hexAddr = ui->checkBoxHexAddress->checkState() == Qt::Checked;
    qs_num.sprintf( b_hexAddr ? "0x%04x" : "%d", addr+i);
    QTableWidgetItem *addrItem  =new QTableWidgetItem( qs_num );
    addrItem->setFlags( addrItem->flags() & ~Qt::ItemIsEditable );
    addrItem->setTextAlignment(Qt::AlignRight);
    ui->regTable->setItem( i, AddrColumn    , addrItem );

    QTableWidgetItem *dataItem =NULL;
    qs_num.sprintf( "0x%04x", data);

    dataItem  =new QTableWidgetItem( qs_num );
    dataItem->setTextAlignment(Qt::AlignRight);
    //    dataItem->setFlags( dataItem->flags() & ~Qt::ItemIsEditable );
    ui->regTable->setItem( i, DataColumnHex , dataItem );

    bool bSign = ui->cbDataDecSign->checkState() == Qt::Checked;

    if ( data != -1 ) {
        dataItem  =new QTableWidgetItem( QString::number( bSign ? (int16_t)data : (uint16_t)data) );
        dataItem->setTextAlignment(Qt::AlignRight);
        //    dataItem->setFlags( dataItem->flags() & ~Qt::ItemIsEditable );
        ui->regTable->setItem( i, DataColumnDec , dataItem );
    }
}

void MainWindow::sendModbusRequest( void )
{
	if( m_tcpActive )
		ui->tcpSettingsWidget->tcpConnect();

	if( m_modbus == NULL )
	{
		setStatusError( tr("Not configured!") );
		return;
	}

	const int slave = ui->slaveID->value();
	const int func = stringToHex( embracedString(
					ui->functionCode->currentText() ) );
	const int addr = ui->startAddr->value();
	int num = ui->numCoils->value();
	uint8_t dest[1024];
	uint16_t * dest16 = (uint16_t *) dest;

	memset( dest, 0, 1024 );

	int ret = -1;
	bool is16Bit = false;
	bool writeAccess = false;
	const QString dataType = descriptiveDataTypeName( func );

	modbus_set_slave( m_modbus, slave );

    LastTick = GetTickCount();
    DataNoByte =0;
	switch( func )
	{
		case MODBUS_FC_READ_COILS:
			ret = modbus_read_bits( m_modbus, addr, num, dest );
			break;
		case MODBUS_FC_READ_DISCRETE_INPUTS:
			ret = modbus_read_input_bits( m_modbus, addr, num, dest );
			break;
		case MODBUS_FC_READ_HOLDING_REGISTERS:
			ret = modbus_read_registers( m_modbus, addr, num, dest16 );
			is16Bit = true;
			break;
		case MODBUS_FC_READ_INPUT_REGISTERS:
			ret = modbus_read_input_registers( m_modbus, addr, num, dest16 );
			is16Bit = true;
			break;
		case MODBUS_FC_WRITE_SINGLE_COIL:
			ret = modbus_write_bit( m_modbus, addr,
                    ui->regTable->item( 0, DataColumnDec )->
						text().toInt(0, 0) ? 1 : 0 );
			writeAccess = true;
			num = 1;
			break;
		case MODBUS_FC_WRITE_SINGLE_REGISTER:
			ret = modbus_write_register( m_modbus, addr,
                    ui->regTable->item( 0, DataColumnDec )->
						text().toInt(0, 0) );
			writeAccess = true;
			num = 1;
			break;

		case MODBUS_FC_WRITE_MULTIPLE_COILS:
		{
			uint8_t * data = new uint8_t[num];
			for( int i = 0; i < num; ++i )
			{
                data[i] = ui->regTable->item( i, DataColumnDec )->
								text().toInt(0, 0);
			}
			ret = modbus_write_bits( m_modbus, addr, num, data );
			delete[] data;
			writeAccess = true;
			break;
		}
		case MODBUS_FC_WRITE_MULTIPLE_REGISTERS:
		{
			uint16_t * data = new uint16_t[num];
			for( int i = 0; i < num; ++i )
			{
                data[i] = ui->regTable->item( i, DataColumnDec )->
								text().toInt(0, 0);
			}
			ret = modbus_write_registers( m_modbus, addr, num, data );
			delete[] data;
			writeAccess = true;
			break;
		}

		default:
			break;
	}

	if( ret == num  )
	{
		if( writeAccess )
		{
			m_statusText->setText(
					tr( "Values successfully sent" ) );
			m_statusInd->setStyleSheet( "background: #0b0;" );
			m_statusTimer->start( 2000 );
		}
		else
		{
            //bool b_hex = is16Bit && ui->checkBoxHexData->checkState() == Qt::Checked;
			QString qs_num;

			ui->regTable->setRowCount( num );
			for( int i = 0; i < num; ++i )
			{
				int data = is16Bit ? dest16[i] : dest[i];
#if 1
	regTable_add( i, is16Bit, addr, dataType, data );
#else
				QTableWidgetItem * dtItem =
					new QTableWidgetItem( dataType );
				QTableWidgetItem * addrItem =
					new QTableWidgetItem(
						QString::number( addr+i ) );
				qs_num.sprintf( b_hex ? "0x%04x" : "%d", data);
				QTableWidgetItem * dataItem =
					new QTableWidgetItem( qs_num );
				dtItem->setFlags( dtItem->flags() &
							~Qt::ItemIsEditable );
				addrItem->setFlags( addrItem->flags() &
							~Qt::ItemIsEditable );
				dataItem->setFlags( dataItem->flags() &
							~Qt::ItemIsEditable );

				ui->regTable->setItem( i, DataTypeColumn,
								dtItem );
				ui->regTable->setItem( i, AddrColumn,
								addrItem );
				ui->regTable->setItem( i, DataColumn,
#endif								dataItem );
			}
		}
	}
	else
	{
		QString err;

		if( ret < 0 )
		{
			if(
#ifdef WIN32
					errno == WSAETIMEDOUT ||
#endif
					errno == EIO
																	)
			{
				err += tr( "I/O error" );
				err += ": ";
				err += tr( "did not receive any data from slave." );
			}
			else
			{
				err += tr( "Protocol error" );
				err += ": ";
				err += tr( "Slave threw exception '" );
				err += modbus_strerror( errno );
				err += tr( "' or function not implemented." );
			}
		}
		else
		{
			err += tr( "Protocol error" );
			err += ": ";
			err += tr( "Number of registers returned does not "
					"match number of registers requested!" );
		}

		if( err.size() > 0 )
			setStatusError( err );
	}
}

void MainWindow::resetStatus( void )
{
	m_statusText->setText( tr( "Ready" ) );
	m_statusInd->setStyleSheet( "background: #aaa;" );
}

void MainWindow::pollForDataOnBus( void )
{
	if( m_modbus )
	{
		modbus_poll( m_modbus );
	}
}


void MainWindow::openBatchProcessor()
{
	BatchProcessor( this, m_modbus ).exec();
}


void MainWindow::aboutQModBus( void )
{
	AboutDialog( this ).exec();
}

void MainWindow::showMacroHelp( void )
{
    MacroHelpDialog( this ).exec();
}

void MainWindow::onRtuPortActive(bool active)
{
	if (active) {
		m_modbus = ui->rtuSettingsWidget->modbus();
		if (m_modbus) {
			modbus_register_monitor_add_item_fnc(m_modbus, MainWindow::stBusMonitorAddItem);
			modbus_register_monitor_raw_data_fnc(m_modbus, MainWindow::stBusMonitorRawData);
		}
		m_tcpActive = false;
	}
	else {
		m_modbus = NULL;
	}
}

void MainWindow::onAsciiPortActive(bool active)
{
    if (active) {
        m_modbus = ui->asciiSettingsWidget->modbus();
        if (m_modbus) {
            modbus_register_monitor_add_item_fnc(m_modbus, MainWindow::stBusMonitorAddItem);
            modbus_register_monitor_raw_data_fnc(m_modbus, MainWindow::stBusMonitorRawData);
        }
        m_tcpActive = false;
    }
    else {
        m_modbus = NULL;
    }
}

void MainWindow::onTcpPortActive(bool active)
{
	m_tcpActive = active;

	if (active) {
		m_modbus = ui->tcpSettingsWidget->modbus();
		if (m_modbus) {
			modbus_register_monitor_add_item_fnc(m_modbus, MainWindow::stBusMonitorAddItem);
			modbus_register_monitor_raw_data_fnc(m_modbus, MainWindow::stBusMonitorRawData);
		}
	}
	else {
		m_modbus = NULL;
	}
}

void MainWindow::setStatusError(const QString &msg)
{
    m_statusText->setText( msg );

    m_statusInd->setStyleSheet( "background: red;" );

    m_statusTimer->start( 2000 );

//    QString txt;
//    txt.sprintf("%s%s%s<br>", spanRed, msg.toStdString().c_str(), spanEnd );
    DataRawAddHtml(QString("%1%2%3<br>").arg(spanRed, msg, spanEnd));
}

void MainWindow::checkBoxHexAddress_toggle( bool hex_dec )
{
    if ( hex_dec ) {
        ui->startAddr->setPrefix("0x");
        ui->startAddr->setDisplayIntegerBase(16);
    } else {
        ui->startAddr->setPrefix("");
        ui->startAddr->setDisplayIntegerBase(10);
    }
}


void MainWindow::on_pbFuncCode_3_clicked()
{
    ui->functionCode->setCurrentIndex(2);
}
void MainWindow::on_pbFuncCode_4_clicked()
{
    ui->functionCode->setCurrentIndex(3);
}
void MainWindow::on_pbFuncCode_6_clicked()
{
    ui->functionCode->setCurrentIndex(5);
}
void MainWindow::on_pbFuncCode_16_clicked()
{
    ui->functionCode->setCurrentIndex(7);
}

void MainWindow::on_regTable_currentCellChanged(int currentRow, int currentColumn, int previousRow, int previousColumn)
{
//    qDebug("*TU*");
}

void MainWindow::on_regTable_itemChanged(QTableWidgetItem *item)
{
  qDebug("*%s*", item->text().toStdString().c_str() );
  QTableWidget *tw = item->tableWidget();
  int iCol = tw->currentColumn();
  int iRow = tw->currentRow();
  if ( !(iCol == DataColumnHex || iCol == DataColumnDec) ) {
      return;
  }
  tw->setDisabled(true);
  char *pEnd;
  uint16_t value = strtoul( item->text().toStdString().c_str(), &pEnd, 0 );
  QString strVal;
  bool bSign = ui->cbDataDecSign->checkState() == Qt::Checked;
  if ( bSign )
    strVal.sprintf("%d",(int16_t)value);
  else
    strVal.sprintf("%u",(uint16_t)value );

//  tw->setState( Qt::);
  tw->item( iRow, DataColumnDec )->setText( strVal );

  strVal.sprintf("0x%04X",(uint16_t)value );
  tw->item( iRow, DataColumnHex )->setText( strVal );
  tw->setDisabled(false);
}

typedef struct {
    uint8_t     Code;
    const char *Name;
} FuncCode_t;

static const FuncCode_t _FuncCode[] = {
    { 0x01, "Read_Coils"              },
    { 0x02, "Read_Discrete_Inputs"    },
    { 0x03, "Read_Holding_Registers"  },
    { 0x04, "Read_Input_Registers"    },
    { 0x05, "Write_Single_Coil"       },
    { 0x06, "Write_Single_Register"   },
    { 0x0f, "Write_Multiple_Coils"    },
    { 0x10, "Write_Multiple_Registers"},
};
#define ARRAY_COUNT( arr ) (sizeof(arr)/sizeof(arr[0]))

uint8_t StrToFuncctionCode( const char *strFuncCode )
{
    int num = ARRAY_COUNT(_FuncCode);
    const FuncCode_t *fn =_FuncCode;
    do {
        if ( stricmp( strFuncCode, fn->Name ) == 0 ) {
            return fn->Code;
        }
        fn ++;
    } while( --num );
    return 0;
}

int QComboBoxFindFuncCode( const QComboBox *cb, uint8_t FuncCode )
{
    for( int i =0; i < cb->count(); i++ ) {
        if ( FuncCode == stringToHex( embracedString( cb->itemText(i) ) ) )
            return i;
    }
    return -1;
}

// .id=<number>,
// .fn=< number | Read_Coils | Read_Discrete_Inputs | Read_Holding_Registers | Read_Input_Registers |
//                Write_Single_Coil | Write_Single_Register | Write_Multiple_Coils |
//                Write_Multiple_Registers >,
// .adr=<number>,
// .siz=<number>,
// [.data='['<number> [,<number>] ']' ]
// .click,
bool MainWindow::MacroToRequest(QString strMacro)
{
    bool result = false;
    QChar zn;
    QString str;

    enum {
        fldNone,
        fldID,
        fldFuncCode,
        fldAddr,
        fldSize,
        fldData,
    };
    int fldNo = fldNone;
    enum {
        stFindDot,
        stReadFld,
        stReadDataStart,
        stReadData,
        stReadParam,
    };
    int state = stFindDot;

    int iRow =0;

    for( int i =0; i <= strMacro.length(); i++ ) {

        zn = strMacro[i];
        switch( state ) {
        case stFindDot:
            if ( zn != '.' ) break;
            state =stReadFld;
            str ="";
            break;
        case stReadFld:
            if ( zn != '=' && zn != ',' && zn != 0 ) {
                str += zn;
                break;
            }
            str = str.toLower();
            fldNo = fldNone;
            state = stReadParam;
            do {
            if ( str == "id"   ) { fldNo =fldID;       break; }
            if ( str == "fn"   ) { fldNo =fldFuncCode; break; }
            if ( str == "adr"  ) { fldNo =fldAddr;     break; }
            if ( str == "size" ) { fldNo =fldSize;     break; }
            if ( str == "data" ) { fldNo =fldData;     state  =stReadDataStart; break; }
            if ( str == "click") { state =stFindDot;   result =true; break; }
            } while( 0 );
            str ="";
            break;
        case stReadParam:{
            if ( zn != ',' &&
                 zn != 0   &&
                 !(fldNo == fldData && zn == ']' ) ) {
                str += zn;
                break;
            }
            state =stFindDot;
            str = str.trimmed();
            char *end;
            int val = strtol( str.toStdString().c_str(), &end, 0);
            switch( fldNo ) {
            case fldNone:
                break;
            case fldID:
                ui->slaveID->setValue(val);
                break;
            case fldFuncCode:
                if ( val == 0 ) {
                    val = StrToFuncctionCode( str.toStdString().c_str() );
                }
                if ( val ) {
                    int i =QComboBoxFindFuncCode( ui->functionCode, val );
                    if ( i != -1 ) {
                      ui->functionCode->setCurrentIndex(i);
                      updateRegisterView();
                    }
                }
                break;
            case fldAddr:
                ui->startAddr->setValue(val);
                break;
            case fldSize:
                ui->numCoils->setValue(val);
                // wait
                updateRegisterView();
                break;
            case fldData:
                if ( !str.isEmpty() ) {
                    if ( iRow < ui->regTable->rowCount() )
                        ui->regTable->item(iRow, DataColumnHex)->setText( str );
                }
                iRow++;
                if ( zn != ']' ) state =stReadParam;
                break;
            }
            str ="";
            break;
            }
        case stReadDataStart:
            if ( zn != '[' ) break;
            state =stReadParam;
            iRow =0;
            str ="";
            break;
        }
    }
    return result;
}
#if 0
void MainWindow::on_MacroButton_clicked()
{
    QPushButton *pb = dynamic_cast<QPushButton *>(sender());
    if ( pb == NULL ) return;

    int no= pb->objectName().section("_",1).toInt();

    QLineEdit *le = findChild<QLineEdit *>( QString().sprintf("leMacroTxt_%u",no) );
    if ( le == NULL ) return;

    QString strMacro = le->text();
    if ( MacroToRequest( strMacro ) ) {
        // mayby click
        ui->sendBtn->click();
    }
}
#endif
#if 0
void MainWindow::on_pbMacroSave_clicked()
{
    QSettings sets;
    QList<QLineEdit *> les =findChildren<QLineEdit *>();
    sets.beginGroup("QLineEdit");
    for( int i = 0; i < les.length(); i++ ) {
        sets.setValue( les[i]->objectName(), les[i]->text() );
    }
    sets.endGroup();
    sets.beginGroup("QSpinBox");
    QList<QSpinBox *> sbs =findChildren<QSpinBox *>();
    for( int i = 0; i < sbs.length(); i++ ) {
        sets.setValue( sbs[i]->objectName(), sbs[i]->value() );
    }
    sets.endGroup();
    sets.beginGroup("QCheckBox");
    QList<QCheckBox *> cbs =findChildren<QCheckBox *>();
    for( int i = 0; i < cbs.length(); i++ ) {
        sets.setValue( cbs[i]->objectName(), cbs[i]->checkState() == Qt::Checked );
    }
    sets.endGroup();
}
#endif
#if 0
void MainWindow::on_pbMacroLoad_clicked()
{
    QSettings sets;
    QList<QLineEdit *> les =findChildren<QLineEdit *>();
    sets.beginGroup("QLineEdit");
    for( int i = 0; i < les.length(); i++ ) {
        les[i]->setText( sets.value(les[i]->objectName()).toString() );
    }
    sets.endGroup();
    sets.beginGroup("QSpinBox");
    QList<QSpinBox *> sbs =findChildren<QSpinBox *>();
    for( int i = 0; i < sbs.length(); i++ ) {
        sbs[i]->setValue( sets.value( sbs[i]->objectName() ).toInt() );
    }
    sets.endGroup();
    sets.beginGroup("QCheckBox");
    QList<QCheckBox *> cbs =findChildren<QCheckBox *>();
    for( int i = 0; i < cbs.length(); i++ ) {
        cbs[i]->setChecked( sets.value( cbs[i]->objectName() ).toInt() );
    }
    sets.endGroup();

}
#endif
