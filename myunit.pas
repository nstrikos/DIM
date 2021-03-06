unit MyUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, bgraimageprocess;

Type
    TMyThread = class(TThread)
            Input : TBGRABitmap;
      fm:extended;
      Output : TBGRABitmap;
      index : Integer;
      pointer : ^TBGRABitmap;
      width: Integer;
    private
      fStatusText : string;
      procedure ShowStatus;
    protected
      procedure Execute; override;
    public
      Constructor Create(CreateSuspended : boolean);
    end;
{
  constructor TMyThread.Create(CreateSuspended : boolean);
  begin
    FreeOnTerminate := True;  // bad code. never do that. reverse these calls...
    inherited Create(CreateSuspended); // because this is black box in OOP and can reset inherited to false again...
  end;
}


implementation

constructor TMyThread.Create(CreateSuspended : boolean);
  begin
    inherited Create(CreateSuspended); // because this is black box in OOP and can reset inherited to the opposite again...
    FreeOnTerminate := True;  // better code...
  end;

  procedure TMyThread.ShowStatus;
  // this method is executed by the mainthread and can therefore access all GUI elements.
  begin
    //Form1.Caption := fStatusText;
  end;

  procedure TMyThread.Execute;
  var
    newStatus : string;
  begin
    fStatusText := 'TMyThread Starting...';
    Synchronize(@Showstatus);
    fStatusText := 'TMyThread Running...';
    //while (not Terminated)  {and ([any condition required]) } do
    //  begin
        //...
        //[here goes the code of the main thread loop]
        //...
        if (index = 1) then
        begin
             BGRABicubicPolyrama1(Input, fm, Output);
        end
        else if (index = 2) then
        begin
             width := width div 4;
             BGRABicubicPolyrama2(Input, fm, pointer, width, 2 * width);
        end
        else if (index = 3) then
        begin
             width := width div 4;
             BGRABicubicPolyrama2(Input, fm, pointer, 2 * width, 3 * width);
        end
        else
        begin
             width := width div 4;
             BGRABicubicPolyrama2(Input, fm, pointer, 3 * width, 4 * width);
        end;


    //    if NewStatus <> fStatusText then
    //      begin
    //        fStatusText := newStatus;
    //        Synchronize(@Showstatus);
    //      end;
    //  end;
  end;

end.

