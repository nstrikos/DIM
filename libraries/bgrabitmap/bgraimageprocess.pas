{
This unit contains the most of the procedures which are used
for processing images.

Copyright (C) 2003 - 2011 Strikos Nikolaos

This file is part of Digital Image Magnifier.

Digital Image Magnifier is free software;
you can redistribute it and/or modify it under the
terms of the GNU General Public License version 2
as published by the Free Software Foundation.

Digital Image Magnifier is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. See the GNU General Public License for more details.

}

unit bgraimageprocess;

interface

uses
     Math, BGRABitmap, BGRABitmapTypes;

     type ImageArray=array of array of single;

     type BitmapPointer = ^TBGRABitmap;

     procedure BGRAPixelRepetition (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
     procedure BGRABilinear (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
     procedure BGRABicubic (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
     procedure BGRABicubicPolyrama (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
     procedure BGRABicubicPolyrama1 (var Input : TBGRABitmap ; fm:extended; var Output : TBGRABitmap);
     procedure BGRABicubicPolyrama2 (var Input : TBGRABitmap;
                                    fm:extended;
                                    var Output : BitmapPointer;
                                    first, final : Integer);
     procedure BGRABicubicPolyrama3 (var Input : TBGRABitmap ; fm:extended; var Output : BitmapPointer);
     procedure BGRABicubicPolyrama4 (var Input : TBGRABitmap ; fm:extended; var Output : BitmapPointer);
     procedure BGRABicubicCatmullRom (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
     procedure BGRAInvertColors(Input : TBGRABitmap);
     procedure BGRASetAlpha(Input : TBGRABitmap);

     function c(x:single):single;         //bicubic filter
     function poly3(x:single):single;  //this function implements a different bicubic filter of panorama tools
     function catmullrom(x:single):single;  //this function implements a different bicubic filter of panorama tools
     function s(x:single):single;         //quadratic filter
     function hf(x:single):single;        //Hermite filter
     function sinc(x:single):single;      //sinc function
     function Lancf(x,a:single):single;     //Lanc filter
     function mf(x:single):single;        //Mitchel filter



implementation

procedure BGRAPixelRepetition (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
var
   x,y:single;
   i,j:int64;
   k,l:integer;
   p, pOutput : PBGRAPixel;
begin
  for l := 0 to Output.Height-1 do
  begin
    y:=(l+0.5)/fm;
    i:=trunc(y);
    if i>=Input.Height then
       i:=Input.Height-1;
    pOutput:=Output.ScanLine[l];
    p := Input.ScanLine[i];
    for k := 0 to Output.Width-1 do
    begin
      x:=(k+0.5)/fm;
      j:=trunc(x);
      if j>=Input.Width then
         j:=Input.Width-1;
      pOutput[k].red:=p[j].red;
      pOutput[k].green:=p[j].green;
      pOutput[k].blue:=p[j].blue;
      pOutput[k].alpha:=255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;

//need to take care of border pixels
procedure BGRABilinear (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
var
   x,y:single;
   a,b:single;
   i,j:int64;
   k,l:integer;
   p1, p2, pOutput : PBGRAPixel;
begin
  for l := 0 to Output.Height-1 do
  begin
    y:=(l+0.5)/fm;
    i:=trunc(y);
    a:=y-i;
    if i>=Input.Height-1 then
       i:=Input.Height-2;
    pOutput:=Output.ScanLine[l];
    p1 := Input.ScanLine[i];
    p2 := Input.ScanLine[i+1];
    for k := 0 to Output.Width-1 do
    begin
      x:=(k+0.5)/fm;
      j:=trunc(x);
      b:=x-j;
      if j>=Input.Width-1 then
         j:=Input.Width-2;
      pOutput[k].red:=Round(p1[j].red*(1-a)*(1-b) + p1[j+1].red*(1-a)*b + p2[j].red*a*(1-b) + p2[j+1].red*a*b);
      pOutput[k].green:=Round(p1[j].green*(1-a)*(1-b) + p1[j+1].green*(1-a)*b + p2[j].green*a*(1-b) + p2[j+1].green*a*b);
      pOutput[k].blue:=Round(p1[j].blue*(1-a)*(1-b) + p1[j+1].blue*(1-a)*b + p2[j].blue*a*(1-b) + p2[j+1].blue*a*b);
      pOutput[k].alpha:=255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;

//need to take care of border pixels
procedure BGRABicubic (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
var
   x,y:single;
   a,b:single;
   i,j:int64;
   k,l:integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;

begin

   SetLength(ca, Output.Width, 4);

   for k := 0 to Output.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=c(1+a);
      ca[k][1]:=c(a);
      ca[k][2]:=c(1-a);
      ca[k][3]:=c(2-a);
   end;

  for l := 0 to Output.Height-1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    if i>=Input.Height-3 then
       i:=Input.Height-4;
    if i<1 then i:=1;
    pOutput:=Output.ScanLine[l];
    p0 := Input.ScanLine[i-1];
    p1 := Input.ScanLine[i];
    p2 := Input.ScanLine[i+1];
    p3 := Input.ScanLine[i+2];
    c5:=c(1+b);
    c6:=c(b);
    c7:=c(1-b);
    c8:=c(2-b);
    for k := 0 to Output.Width-1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];
      if j>=Input.Width-1 then
         j:=Input.Width-2;
      pOutput[k].red:= Round(c5*(p0[j-1].red*c1 + p0[j].red*c2 + p0[j+1].red*c3 + p0[j+2].red*c4)
                           + c6*(p1[j-1].red*c1 + p1[j].red*c2 + p1[j+1].red*c3 + p1[j+2].red*c4)
                           + c7*(p2[j-1].red*c1 + p2[j].red*c2 + p2[j+1].red*c3 + p2[j+2].red*c4)
                           + c8*(p3[j-1].red*c1 + p3[j].red*c2 + p3[j+1].red*c3 + p3[j+2].red*c4));

      pOutput[k].green:= Round(c5*(p0[j-1].green*c1 + p0[j].green*c2 + p0[j+1].green*c3 + p0[j+2].green*c4)
                             + c6*(p1[j-1].green*c1 + p1[j].green*c2 + p1[j+1].green*c3 + p1[j+2].green*c4)
                             + c7*(p2[j-1].green*c1 + p2[j].green*c2 + p2[j+1].green*c3 + p2[j+2].green*c4)
                             + c8*(p3[j-1].green*c1 + p3[j].green*c2 + p3[j+1].green*c3 + p3[j+2].green*c4));

      pOutput[k].blue:= Round(c5*(p0[j-1].blue*c1 + p0[j].blue*c2 + p0[j+1].blue*c3 + p0[j+2].blue*c4)
                            + c6*(p1[j-1].blue*c1 + p1[j].blue*c2 + p1[j+1].blue*c3 + p1[j+2].blue*c4)
                            + c7*(p2[j-1].blue*c1 + p2[j].blue*c2 + p2[j+1].blue*c3 + p2[j+2].blue*c4)
                            + c8*(p3[j-1].blue*c1 + p3[j].blue*c2 + p3[j+1].blue*c3 + p3[j+2].blue*c4));
      pOutput[k].alpha:=255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;

procedure BGRABicubicPolyrama (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
var
   x,y : single;
   a,b : single;
   i,j : int64;
   k,l : integer;
   j1, j3, j4 : integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   sum :single;
begin

   SetLength(ca, Output.Width, 4);

   for k := 0 to Output.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=poly3(1+a);
      ca[k][1]:=poly3(a);
      ca[k][2]:=poly3(1-a);
      ca[k][3]:=poly3(2-a);
   end;

  for l := 0 to Output.Height-1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    pOutput:=Output.ScanLine[l];

    //take care of border pixels
    if i>0 then p0 := Input.ScanLine[i-1]
    else p0 := Input.ScanLine[0];

    p1 := Input.ScanLine[i];

    if i<Input.Height-1 then p2 := Input.ScanLine[i+1]
    else p2 := Input.ScanLine[Input.Height-1];

    if i<Input.Height-2 then p3 := Input.ScanLine[i+2]
    else p3 := Input.ScanLine[Input.Height-1];

    //calculate factors
    c5:=poly3(1+b);
    c6:=poly3(b);
    c7:=poly3(1-b);
    c8:=poly3(2-b);
    for k := 0 to Output.Width-1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];

      //take care of border pixels
      if j>0 then j1:=j-1
      else j1:=0;

      if j<Input.Width-1 then j3:=j+1
      else j3:=Input.Width-1;

      if j<Input.Width-2 then j4:=j+2
      else j4:=Input.Width-1;

      Sum :=  Round(c5*(p0[j1].red*c1 + p0[j].red*c2 + p0[j3].red*c3 + p0[j4].red*c4)
                  + c6*(p1[j1].red*c1 + p1[j].red*c2 + p1[j3].red*c3 + p1[j4].red*c4)
                  + c7*(p2[j1].red*c1 + p2[j].red*c2 + p2[j3].red*c3 + p2[j4].red*c4)
                  + c8*(p3[j1].red*c1 + p3[j].red*c2 + p3[j3].red*c3 + p3[j4].red*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].red:= Round(sum);

      sum := Round(c5*(p0[j].green*c1 + p0[j].green*c2 + p0[j3].green*c3 + p0[j4].green*c4)
                 + c6*(p1[j].green*c1 + p1[j].green*c2 + p1[j3].green*c3 + p1[j4].green*c4)
                 + c7*(p2[j].green*c1 + p2[j].green*c2 + p2[j3].green*c3 + p2[j4].green*c4)
                 + c8*(p3[j].green*c1 + p3[j].green*c2 + p3[j3].green*c3 + p3[j4].green*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].green:= Round(sum);

      sum := Round(c5*(p0[j].blue*c1 + p0[j].blue*c2 + p0[j3].blue*c3 + p0[j4].blue*c4)
                 + c6*(p1[j].blue*c1 + p1[j].blue*c2 + p1[j3].blue*c3 + p1[j4].blue*c4)
                 + c7*(p2[j].blue*c1 + p2[j].blue*c2 + p2[j3].blue*c3 + p2[j4].blue*c4)
                 + c8*(p3[j].blue*c1 + p3[j].blue*c2 + p3[j3].blue*c3 + p3[j4].blue*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].blue:= Round(sum);

      pOutput[k].alpha:=255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;

procedure BGRABicubicPolyrama1 (var Input : TBGRABitmap ; fm:extended; var Output : TBGRABitmap);
var
   x,y : single;
   a,b : single;
   i,j : int64;
   k,l : integer;
   j1, j3, j4 : integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   sum :single;
   customWidth : Integer;
begin

   customWidth := Output.Width div 4;

   SetLength(ca, Output.Width, 4);

   //customWidth := Output.Width div 2;

   for k := 0 to Output.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=poly3(1+a);
      ca[k][1]:=poly3(a);
      ca[k][2]:=poly3(1-a);
      ca[k][3]:=poly3(2-a);
   end;

  for l := 0 to Output.Height-1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    pOutput:=Output.ScanLine[l];

    //take care of border pixels
    if i>0 then p0 := Input.ScanLine[i-1]
    else p0 := Input.ScanLine[0];

    p1 := Input.ScanLine[i];

    if i<Input.Height-1 then p2 := Input.ScanLine[i+1]
    else p2 := Input.ScanLine[Input.Height-1];

    if i<Input.Height-2 then p3 := Input.ScanLine[i+2]
    else p3 := Input.ScanLine[Input.Height-1];

    //calculate factors
    c5:=poly3(1+b);
    c6:=poly3(b);
    c7:=poly3(1-b);
    c8:=poly3(2-b);
    for k := 0 to customWidth - 1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];

      //take care of border pixels
      if j>0 then j1:=j-1
      else j1:=0;

      if j<Input.Width-1 then j3:=j+1
      else j3:=Input.Width-1;

      if j<Input.Width-2 then j4:=j+2
      else j4:=Input.Width-1;

      Sum :=  Round(c5*(p0[j1].red*c1 + p0[j].red*c2 + p0[j3].red*c3 + p0[j4].red*c4)
                  + c6*(p1[j1].red*c1 + p1[j].red*c2 + p1[j3].red*c3 + p1[j4].red*c4)
                  + c7*(p2[j1].red*c1 + p2[j].red*c2 + p2[j3].red*c3 + p2[j4].red*c4)
                  + c8*(p3[j1].red*c1 + p3[j].red*c2 + p3[j3].red*c3 + p3[j4].red*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].red:= Round(sum);

      sum := Round(c5*(p0[j].green*c1 + p0[j].green*c2 + p0[j3].green*c3 + p0[j4].green*c4)
                 + c6*(p1[j].green*c1 + p1[j].green*c2 + p1[j3].green*c3 + p1[j4].green*c4)
                 + c7*(p2[j].green*c1 + p2[j].green*c2 + p2[j3].green*c3 + p2[j4].green*c4)
                 + c8*(p3[j].green*c1 + p3[j].green*c2 + p3[j3].green*c3 + p3[j4].green*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].green:= Round(sum);

      sum := Round(c5*(p0[j].blue*c1 + p0[j].blue*c2 + p0[j3].blue*c3 + p0[j4].blue*c4)
                 + c6*(p1[j].blue*c1 + p1[j].blue*c2 + p1[j3].blue*c3 + p1[j4].blue*c4)
                 + c7*(p2[j].blue*c1 + p2[j].blue*c2 + p2[j3].blue*c3 + p2[j4].blue*c4)
                 + c8*(p3[j].blue*c1 + p3[j].blue*c2 + p3[j3].blue*c3 + p3[j4].blue*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].blue:= Round(sum);

      pOutput[k].alpha:=255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;

procedure BGRABicubicPolyrama2 (var Input : TBGRABitmap ;
                               fm:extended;
                               var Output : BitmapPointer;
                               first, final : Integer);
var
   x,y : single;
   a,b : single;
   i,j : int64;
   k,l : integer;
   j1, j3, j4 : integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   sum :single;
   startP, endP, customWidth : Integer;
begin

   customWidth := Output^.Width div 4;
   //startP := customWidth;
   //endP := Output^.Width div 2;
   startP := first;
   endP := final;
   //writeln(startP);
   //writeln(endP);

   SetLength(ca, Output^.Width, 4);

   for k := 0 to Output^.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=poly3(1+a);
      ca[k][1]:=poly3(a);
      ca[k][2]:=poly3(1-a);
      ca[k][3]:=poly3(2-a);
   end;

  for l := 0 to Output^.Height-1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    pOutput:=Output^.ScanLine[l];

    //take care of border pixels
    if i>0 then p0 := Input.ScanLine[i-1]
    else p0 := Input.ScanLine[0];

    p1 := Input.ScanLine[i];

    if i<Input.Height-1 then p2 := Input.ScanLine[i+1]
    else p2 := Input.ScanLine[Input.Height-1];

    if i<Input.Height-2 then p3 := Input.ScanLine[i+2]
    else p3 := Input.ScanLine[Input.Height-1];

    //calculate factors
    c5:=poly3(1+b);
    c6:=poly3(b);
    c7:=poly3(1-b);
    c8:=poly3(2-b);
    for k := startP to endP-1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];

      //take care of border pixels
      if j>0 then j1:=j-1
      else j1:=0;

      if j<Input.Width-1 then j3:=j+1
      else j3:=Input.Width-1;

      if j<Input.Width-2 then j4:=j+2
      else j4:=Input.Width-1;

      Sum :=  Round(c5*(p0[j1].red*c1 + p0[j].red*c2 + p0[j3].red*c3 + p0[j4].red*c4)
                  + c6*(p1[j1].red*c1 + p1[j].red*c2 + p1[j3].red*c3 + p1[j4].red*c4)
                  + c7*(p2[j1].red*c1 + p2[j].red*c2 + p2[j3].red*c3 + p2[j4].red*c4)
                  + c8*(p3[j1].red*c1 + p3[j].red*c2 + p3[j3].red*c3 + p3[j4].red*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].red:= Round(sum);

      sum := Round(c5*(p0[j].green*c1 + p0[j].green*c2 + p0[j3].green*c3 + p0[j4].green*c4)
                 + c6*(p1[j].green*c1 + p1[j].green*c2 + p1[j3].green*c3 + p1[j4].green*c4)
                 + c7*(p2[j].green*c1 + p2[j].green*c2 + p2[j3].green*c3 + p2[j4].green*c4)
                 + c8*(p3[j].green*c1 + p3[j].green*c2 + p3[j3].green*c3 + p3[j4].green*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].green:= Round(sum);

      sum := Round(c5*(p0[j].blue*c1 + p0[j].blue*c2 + p0[j3].blue*c3 + p0[j4].blue*c4)
                 + c6*(p1[j].blue*c1 + p1[j].blue*c2 + p1[j3].blue*c3 + p1[j4].blue*c4)
                 + c7*(p2[j].blue*c1 + p2[j].blue*c2 + p2[j3].blue*c3 + p2[j4].blue*c4)
                 + c8*(p3[j].blue*c1 + p3[j].blue*c2 + p3[j3].blue*c3 + p3[j4].blue*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].blue:= Round(sum);

      pOutput[k].alpha:=255;
    end;
  end;
  Output^.InvalidateBitmap; // changed by direct access
end;

procedure BGRABicubicPolyrama3 (var Input : TBGRABitmap ; fm:extended; var Output : BitmapPointer);
var
   x,y : single;
   a,b : single;
   i,j : int64;
   k,l : integer;
   j1, j3, j4 : integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   sum :single;
   startP, endP, customWidth : Integer;
begin

   customWidth := Output^.Width div 4;
   startP := Output^.Width div 2;
   endP := startP + customWidth;

   SetLength(ca, Output^.Width, 4);

   for k := 0 to Output^.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=poly3(1+a);
      ca[k][1]:=poly3(a);
      ca[k][2]:=poly3(1-a);
      ca[k][3]:=poly3(2-a);
   end;

  for l := 0 to Output^.Height-1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    pOutput:=Output^.ScanLine[l];

    //take care of border pixels
    if i>0 then p0 := Input.ScanLine[i-1]
    else p0 := Input.ScanLine[0];

    p1 := Input.ScanLine[i];

    if i<Input.Height-1 then p2 := Input.ScanLine[i+1]
    else p2 := Input.ScanLine[Input.Height-1];

    if i<Input.Height-2 then p3 := Input.ScanLine[i+2]
    else p3 := Input.ScanLine[Input.Height-1];

    //calculate factors
    c5:=poly3(1+b);
    c6:=poly3(b);
    c7:=poly3(1-b);
    c8:=poly3(2-b);
    for k := startP to endP-1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];

      //take care of border pixels
      if j>0 then j1:=j-1
      else j1:=0;

      if j<Input.Width-1 then j3:=j+1
      else j3:=Input.Width-1;

      if j<Input.Width-2 then j4:=j+2
      else j4:=Input.Width-1;

      Sum :=  Round(c5*(p0[j1].red*c1 + p0[j].red*c2 + p0[j3].red*c3 + p0[j4].red*c4)
                  + c6*(p1[j1].red*c1 + p1[j].red*c2 + p1[j3].red*c3 + p1[j4].red*c4)
                  + c7*(p2[j1].red*c1 + p2[j].red*c2 + p2[j3].red*c3 + p2[j4].red*c4)
                  + c8*(p3[j1].red*c1 + p3[j].red*c2 + p3[j3].red*c3 + p3[j4].red*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].red:= Round(sum);

      sum := Round(c5*(p0[j].green*c1 + p0[j].green*c2 + p0[j3].green*c3 + p0[j4].green*c4)
                 + c6*(p1[j].green*c1 + p1[j].green*c2 + p1[j3].green*c3 + p1[j4].green*c4)
                 + c7*(p2[j].green*c1 + p2[j].green*c2 + p2[j3].green*c3 + p2[j4].green*c4)
                 + c8*(p3[j].green*c1 + p3[j].green*c2 + p3[j3].green*c3 + p3[j4].green*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].green:= Round(sum);

      sum := Round(c5*(p0[j].blue*c1 + p0[j].blue*c2 + p0[j3].blue*c3 + p0[j4].blue*c4)
                 + c6*(p1[j].blue*c1 + p1[j].blue*c2 + p1[j3].blue*c3 + p1[j4].blue*c4)
                 + c7*(p2[j].blue*c1 + p2[j].blue*c2 + p2[j3].blue*c3 + p2[j4].blue*c4)
                 + c8*(p3[j].blue*c1 + p3[j].blue*c2 + p3[j3].blue*c3 + p3[j4].blue*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].blue:= Round(sum);

      pOutput[k].alpha:=255;
    end;
  end;
  Output^.InvalidateBitmap; // changed by direct access
end;

procedure BGRABicubicPolyrama4 (var Input : TBGRABitmap ; fm:extended; var Output : BitmapPointer);
var
   x,y : single;
   a,b : single;
   i,j : int64;
   k,l : integer;
   j1, j3, j4 : integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   sum :single;
   startP, endP, customWidth : Integer;
begin

   customWidth := Output^.Width div 4;
   startP := 3 * customWidth;
   endP := Output^.Width;

   SetLength(ca, Output^.Width, 4);

   for k := 0 to Output^.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=poly3(1+a);
      ca[k][1]:=poly3(a);
      ca[k][2]:=poly3(1-a);
      ca[k][3]:=poly3(2-a);
   end;

  for l := 0 to Output^.Height-1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    pOutput:=Output^.ScanLine[l];

    //take care of border pixels
    if i>0 then p0 := Input.ScanLine[i-1]
    else p0 := Input.ScanLine[0];

    p1 := Input.ScanLine[i];

    if i<Input.Height-1 then p2 := Input.ScanLine[i+1]
    else p2 := Input.ScanLine[Input.Height-1];

    if i<Input.Height-2 then p3 := Input.ScanLine[i+2]
    else p3 := Input.ScanLine[Input.Height-1];

    //calculate factors
    c5:=poly3(1+b);
    c6:=poly3(b);
    c7:=poly3(1-b);
    c8:=poly3(2-b);
    for k := startP to endP-1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];

      //take care of border pixels
      if j>0 then j1:=j-1
      else j1:=0;

      if j<Input.Width-1 then j3:=j+1
      else j3:=Input.Width-1;

      if j<Input.Width-2 then j4:=j+2
      else j4:=Input.Width-1;

      Sum :=  Round(c5*(p0[j1].red*c1 + p0[j].red*c2 + p0[j3].red*c3 + p0[j4].red*c4)
                  + c6*(p1[j1].red*c1 + p1[j].red*c2 + p1[j3].red*c3 + p1[j4].red*c4)
                  + c7*(p2[j1].red*c1 + p2[j].red*c2 + p2[j3].red*c3 + p2[j4].red*c4)
                  + c8*(p3[j1].red*c1 + p3[j].red*c2 + p3[j3].red*c3 + p3[j4].red*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].red:= Round(sum);

      sum := Round(c5*(p0[j].green*c1 + p0[j].green*c2 + p0[j3].green*c3 + p0[j4].green*c4)
                 + c6*(p1[j].green*c1 + p1[j].green*c2 + p1[j3].green*c3 + p1[j4].green*c4)
                 + c7*(p2[j].green*c1 + p2[j].green*c2 + p2[j3].green*c3 + p2[j4].green*c4)
                 + c8*(p3[j].green*c1 + p3[j].green*c2 + p3[j3].green*c3 + p3[j4].green*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].green:= Round(sum);

      sum := Round(c5*(p0[j].blue*c1 + p0[j].blue*c2 + p0[j3].blue*c3 + p0[j4].blue*c4)
                 + c6*(p1[j].blue*c1 + p1[j].blue*c2 + p1[j3].blue*c3 + p1[j4].blue*c4)
                 + c7*(p2[j].blue*c1 + p2[j].blue*c2 + p2[j3].blue*c3 + p2[j4].blue*c4)
                 + c8*(p3[j].blue*c1 + p3[j].blue*c2 + p3[j3].blue*c3 + p3[j4].blue*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].blue:= Round(sum);

      pOutput[k].alpha:=255;
    end;
  end;
  Output^.InvalidateBitmap; // changed by direct access
end;

//need to take care of border pixels
procedure BGRABicubicCatmullRom (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
var
   x,y:single;
   a,b:single;
   i,j:int64;
   k,l:integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   sum :single;
begin

   SetLength(ca, Output.Width, 4);

   for k := 0 to Output.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=catmullrom(1+a);
      ca[k][1]:=catmullrom(a);
      ca[k][2]:=catmullrom(1-a);
      ca[k][3]:=catmullrom(2-a);
   end;

  for l := 0 to Output.Height-1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    if i>=Input.Height-3 then
       i:=Input.Height-4;
    if i<1 then i:=1;
    pOutput:=Output.ScanLine[l];
    p0 := Input.ScanLine[i-1];
    p1 := Input.ScanLine[i];
    p2 := Input.ScanLine[i+1];
    p3 := Input.ScanLine[i+2];
    c5:=catmullrom(1+b);
    c6:=catmullrom(b);
    c7:=catmullrom(1-b);
    c8:=catmullrom(2-b);
    for k := 0 to Output.Width-1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];
      if j>=Input.Width-1 then
         j:=Input.Width-2;
      Sum :=  Round(c5*(p0[j-1].red*c1 + p0[j].red*c2 + p0[j+1].red*c3 + p0[j+2].red*c4)
                           + c6*(p1[j-1].red*c1 + p1[j].red*c2 + p1[j+1].red*c3 + p1[j+2].red*c4)
                           + c7*(p2[j-1].red*c1 + p2[j].red*c2 + p2[j+1].red*c3 + p2[j+2].red*c4)
                           + c8*(p3[j-1].red*c1 + p3[j].red*c2 + p3[j+1].red*c3 + p3[j+2].red*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].red:= Round(sum);

      sum := Round(c5*(p0[j-1].green*c1 + p0[j].green*c2 + p0[j+1].green*c3 + p0[j+2].green*c4)
                             + c6*(p1[j-1].green*c1 + p1[j].green*c2 + p1[j+1].green*c3 + p1[j+2].green*c4)
                             + c7*(p2[j-1].green*c1 + p2[j].green*c2 + p2[j+1].green*c3 + p2[j+2].green*c4)
                             + c8*(p3[j-1].green*c1 + p3[j].green*c2 + p3[j+1].green*c3 + p3[j+2].green*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].green:= Round(sum);

      sum := Round(c5*(p0[j-1].blue*c1 + p0[j].blue*c2 + p0[j+1].blue*c3 + p0[j+2].blue*c4)
                            + c6*(p1[j-1].blue*c1 + p1[j].blue*c2 + p1[j+1].blue*c3 + p1[j+2].blue*c4)
                            + c7*(p2[j-1].blue*c1 + p2[j].blue*c2 + p2[j+1].blue*c3 + p2[j+2].blue*c4)
                            + c8*(p3[j-1].blue*c1 + p3[j].blue*c2 + p3[j+1].blue*c3 + p3[j+2].blue*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].blue:= Round(sum);

      pOutput[k].alpha:=255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;

procedure BGRAInvertColors(Input : TBGRABitmap);
var
   i, j : integer;
   p : PBGRAPixel;
begin
  for i := 0 to Input.Height-1 do
  begin
    p := Input.ScanLine[i];
    for j := 0 to Input.Width-1 do
    begin
      p[j].red:=255 - p[j].red;
      p[j].green:=255 - p[j].green;
      p[j].blue:=255 - p[j].blue;
      p[j].alpha:=255;
    end;
  end;
  Input.InvalidateBitmap; // changed by direct access
end;

procedure BGRASetAlpha(Input : TBGRABitmap);
var
   i, j : integer;
   p : PBGRAPixel;
begin
  for i := 0 to Input.Height-1 do
  begin
    p := Input.ScanLine[i];
    for j := 0 to Input.Width-1 do
    begin
      p[j].alpha:=255;
    end;
  end;
  Input.InvalidateBitmap; // changed by direct access
end;

function c(x:single):single;  //this function implements the bicubic filter
begin
    if (abs(x)>=0) and (abs(x)<=1) then
     Result:=0.5*abs(x*x*x)-sqr(x)+2/3
    else if (abs(x)>1) and (abs(x)<=2) then
     Result:=-1/6*abs(x*x*x)+sqr(x)-2*x+4/3
    else Result:=0;
end;

function poly3(x:single):single;  //this function implements a different bicubic filter of panorama tools
var
  A : single;
begin
    A:=-0.75;
    if (abs(x)>=0) and (abs(x)<=1) then
     Result:=(A+2)*x*x*x - (A+3)*x*x + 1
    else if (abs(x)>1) and (abs(x)<=2) then
     Result:=A*x*x*x - 5*A*x*x + 8*A*x - 4*A
    else Result:=0;
end;

function catmullrom(x:single):single;  //this function implements catmull-rom bicubic filter
var
  A, B : single;
begin
    A:=1-abs(x);
    B:=2-abs(x);
    if (abs(x)>=0) and (abs(x)<=1) then
     Result:=0.5*(-3*A*A*A + 4*A*A + A)
    else if (abs(x)>1) and (abs(x)<=2) then
     Result:=0.5*(B*B*B-B*B)
    else Result:=0;
end;

function s(x:single):single;  //this function implements the quadratic filter
begin
    if (abs(x)>=0) and (abs(x)<=0.5) then
     Result:=-sqr(x)+0.75
    else if (abs(x)>0.5) and (abs(x)<=1.5) then
     Result:=0.5*sqr(x)-1.5*abs(x)+9/8
    else Result:=0;

end;

function hf(x:single):single;  //this function implements the hermite filter
begin
    if abs(x)<=1 then
       Result:=2*(abs(x)*abs(x)*abs(x))-3*(abs(x)*abs(x))+1
    else Result:=0;
end;

function Lancf(x,a:single):single; //this function implements the lanc filter
begin
    if abs(x)<=a then
    begin
      if x<>0 then Result := a*sin(Pi*x)*sin(Pi/a*x)/(Pi*Pi*x*x)
      else Result:=1;
    end
    else Result:=0;
end;

function sinc(x:single):single;     //the sinc function
begin
    if x<>0 then result:=sin(Pi*x)/(Pi*x)
    else Result:=1;
end;

function mf(x:single):single; //this function implements the mitchel filter;
begin
    if abs(x)<=1 then
       Result:=1/6*(7*abs(x)*abs(x)*abs(x)-12*x*x+16/3)
    else if (abs(x)>1) and (abs(x)<2) then
         Result:=1/6*(-7/3*abs(x*x*x)+12*x*x-20*abs(x)+32/3)
    else Result:=0;
end;

end.
