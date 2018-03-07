unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtDlgs,
  ExtCtrls, StdCtrls, Menus,
  BGRABitmap, BGRABitmapTypes, bgraimageprocess,
  MyUnit;

type

  { TForm1 }

	TForm1 = class(TForm)
  		Image: TImage;
    	MainMenu: TMainMenu;
    	FileMItem: TMenuItem;
    	SaveMItem: TMenuItem;
    	SavePictureDialog: TSavePictureDialog;
    	TestMItem: TMenuItem;
    	MenuItem10: TMenuItem;
    	MenuItem11: TMenuItem;
    	MenuItem12: TMenuItem;
    	MenuItem13: TMenuItem;
    	MenuItem14: TMenuItem;
    	OpenMItem: TMenuItem;
    	MenuItem3: TMenuItem;
    	MenuItem4: TMenuItem;
    	MenuItem5: TMenuItem;
    	MenuItem6: TMenuItem;
    	MenuItem7: TMenuItem;
    	MenuItem8: TMenuItem;
    	MenuItem9: TMenuItem;
    	OpenPictureDialog: TOpenPictureDialog;
    	ScrollBox: TScrollBox;
    	procedure FormCreate(Sender: TObject);
    	procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    	procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure MenuItem3Click(Sender: TObject);
    	procedure OpenMItemClick(Sender: TObject);
    	procedure SaveMItemClick(Sender: TObject);
    	procedure TestMItemClick(Sender: TObject);

	private
    	mouseIsDown : Boolean;
    	startDragX : Integer;
    	startDragY : Integer;
        MyThread : TMyThread;
        MyThread2 : TMyThread;
        count : Integer;
        procedure OnThreadDone(ASender : TObject);
        //image2, stretched: TBGRABitmap;
	public

  end;

var
	Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
	ScrollBox.Align := alClient;
    ScrollBox.HorzScrollBar.Tracking := true;
    ScrollBox.VertScrollBar.Tracking := true;
    mouseIsDown := false;
    MyThread := TMyThread.Create(True);
    MyThread2 := TMyThread.Create(True);
    count := 0;
end;

procedure TForm1.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	mouseIsDown := true;
    startDragX := X;
    startDragY := Y;
    Image.Cursor := crHandPoint;
end;

procedure TForm1.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
	distanceX : Integer;
	distanceY : Integer;
begin
	if (mouseIsDown = true) then
	begin
    	distanceX := startDragX - X;
    	distanceY := startDragY - Y;
    	ScrollBox.HorzScrollBar.Position := ScrollBox.HorzScrollBar.Position + distanceX;
    	ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Position + distanceY;
  	end;
end;

procedure TForm1.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	mouseIsDown := false;
    Image.Cursor := crDefault;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
	Close;
end;

procedure TForm1.OpenMItemClick(Sender: TObject);
var
	pic : TPicture;
begin
	if (OpenPictureDialog.Execute) then
    begin
    	pic := TPicture.Create;
        pic.LoadFromFile(OpenPictureDialog.FileName);
        Image.Picture.Bitmap.SetSize(pic.Width, pic.Height);
        Image.Picture.Bitmap.Canvas.Draw(0, 0, pic.Bitmap);
        Image.Width := pic.Width;
        Image.Height := pic.Height;
        ScrollBox.HorzScrollBar.Position := 0;
        ScrollBox.VertScrollBar.Position := 0;
        pic.Free;
        Form1.Caption := 'Clear';
	end;
end;

procedure TForm1.SaveMItemClick(Sender: TObject);
begin
	if (SavePictureDialog.Execute) then
  	begin
    	Image.Picture.SaveToFile(SavePictureDialog.FileName);
  	end;
end;

procedure TForm1.TestMItemClick(Sender: TObject);
var
  	image2, stretched: TBGRABitmap;
  	viewRect : TRect;
  	fm : Real;
begin
    fm := 5;
 {   image2 := TBGRABitmap.Create(Image.Width, Image.Height);
    stretched := TBGRABitmap.Create( Round(Image.Width * fm), Round(Image.Height * fm) );
    viewRect := TRect.Create(0, 0, Image.Width, Image.Height);
    image2.Canvas.CopyRect(viewRect, Image.Picture.Bitmap.Canvas, viewRect);
    //BGRABicubicPolyrama1(image2, fm, stretched);
    //BGRABicubicPolyrama2(image2, fm, stretched);
    BGRABicubicPolyrama(image2, fm, stretched);    }

    MyThread.Input := TBGRABitmap.Create(Image.Width, Image.Height);
    MyThread.Output := TBGRABitmap.Create( Round(Image.Width * fm), Round(Image.Height * fm) );
    viewRect := TRect.Create(0, 0, Image.Width, Image.Height);
    MyThread.Input.Canvas.CopyRect(viewRect, Image.Picture.Bitmap.Canvas, viewRect);
    MyThread.fm := fm;
    MyThread.index:=1;
    MyThread.OnTerminate := @OnThreadDone;
    MyThread.Start;
    MyThread2.Input := TBGRABitmap.Create(Image.Width, Image.Height);
    MyThread2.Output := TBGRABitmap.Create( Round(Image.Width * fm), Round(Image.Height * fm) );
    viewRect := TRect.Create(0, 0, Image.Width, Image.Height);
    MyThread2.Input.Canvas.CopyRect(viewRect, Image.Picture.Bitmap.Canvas, viewRect);
    MyThread2.fm := fm;
    MyThread2.index:=2;
    MyThread2.OnTerminate := @OnThreadDone;
    MyThread2.Start;
  {  Image.Picture.Bitmap.Width := stretched.Width;
    Image.Picture.Bitmap.Height := stretched.Height;
    Image.Height := stretched.Height;
    Image.Width := stretched.Width;
    stretched.Draw(Image.Canvas, 0, 0, True);
    ScrollBox.HorzScrollBar.Position := 0;
    ScrollBox.VertScrollBar.Position := 0;
    image2.free;
    stretched.free;    }
end;

procedure TForm1.OnThreadDone(ASender : TObject);
begin
  count := count + 1;
  if (count = 2) then
  begin
       Form1.Caption := 'Done!';
       Image.Picture.Bitmap.Width := MyThread.Output.Width;//stretched.Width;
       Image.Picture.Bitmap.Height := MyThread.Output.Height;//stretched.Height;
       Image.Height := MyThread.Output.Height;//stretched.Height;
       Image.Width := MyThread.Output.Width;
       MyThread.Output.Draw(Image.Canvas, 0, 0, True);
       //MyThread2.Output.Draw(Image.Canvas, 0, 0, True);
       ScrollBox.HorzScrollBar.Position := 0;
       ScrollBox.VertScrollBar.Position := 0;
       MyThread.Input.free;
       MyThread.Output.free;
  end;
end;

end.

