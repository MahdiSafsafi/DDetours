// **************************************************************************************************
// Delphi Detours Library
// Sample Demo uDockCaptionDrawer.pas
// http://code.google.com/p/delphi-detours-library/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uDockCaptionDrawer.pas.
//
// Author : Rodrigo Ruz.
//
// **************************************************************************************************

unit uDockCaptionDrawer;

interface

implementation


uses
 Types,
 Windows,
 Graphics,
 CaptionedDockTree,
 PngImage,
 GraphUtil,
 Forms,
 DDetours;

{$R Dockimages.RES}

type
 TDockCaptionDrawerClass = class(TDockCaptionDrawer);
var
  Trampoline_TDockCaptionDrawer_DrawDockCaption      : function (Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest =nil;
  {$IF CompilerVersion>=27}
  Trampoline_ModernDockCaptionDrawer_DrawDockCaption : function (Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest =nil;
  {$ENDIF}
  DockerFontColor          : TColor = clBlack;
  DockerBorderColor        : TColor = clBlack;
  DockerStartEnabledColor  : TColor = clWebIvory;
  DockerEndEnabledColor    : TColor = clWebPapayaWhip;
  DockerStartDisabledColor : TColor = clSilver;
  DockerEndDisabledColor   : TColor = clSilver;

{$IF CompilerVersion>=27}
  ModernThemeModule           : HMODULE;
  pModernThemeDrawDockCaption : Pointer;
{$ENDIF}

function CustomDrawDockCaption(Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest;

  procedure DrawIcon;
  var
    FormBitmap: TBitmap;
    DestBitmap: TBitmap;
    ImageSize: Integer;
    X, Y: Integer;
  begin
    if (State.Icon <> nil) and (State.Icon.HandleAllocated) then
    begin
      if Self.DockCaptionOrientation = dcoHorizontal then
      begin
        ImageSize := CaptionRect.Bottom - CaptionRect.Top - 3;
        X := CaptionRect.Left;
        Y := CaptionRect.Top + 2;
      end
      else
      begin
        ImageSize := CaptionRect.Right - CaptionRect.Left - 3;
        X := CaptionRect.Left + 1;
        Y := CaptionRect.Top;
      end;

      FormBitmap := nil;
      DestBitmap := TBitmap.Create;
      try
        FormBitmap := TBitmap.Create;
        DestBitmap.Width :=  ImageSize;
        DestBitmap.Height := ImageSize;
        DestBitmap.Canvas.Brush.Color := clFuchsia;
        DestBitmap.Canvas.FillRect(Rect(0, 0, DestBitmap.Width, DestBitmap.Height));
        FormBitmap.Width := State.Icon.Width;
        FormBitmap.Height := State.Icon.Height;
        FormBitmap.Canvas.Draw(0, 0, State.Icon);
        ScaleImage(FormBitmap, DestBitmap, DestBitmap.Width / FormBitmap.Width);

        DestBitmap.TransparentColor := DestBitmap.Canvas.Pixels[0, DestBitmap.Height - 1];
        DestBitmap.Transparent := True;

        Canvas.Draw(X, Y, DestBitmap);
      finally
        FormBitmap.Free;
        DestBitmap.Free;
      end;

      if Self.DockCaptionOrientation = dcoHorizontal then
        CaptionRect.Left := CaptionRect.Left + 6 + ImageSize
      else
        CaptionRect.Top := CaptionRect.Top + 6 + ImageSize;
    end;
  end;

  function CalcButtonSize(const CaptionRect: TRect): Integer;
  const
    cButtonBuffer = 8;
  begin
    if Self.DockCaptionOrientation = dcoHorizontal then
      Result := CaptionRect.Bottom - CaptionRect.Top - cButtonBuffer
    else
      Result := CaptionRect.Right - CaptionRect.Left - cButtonBuffer;
  end;

  function GetCloseRect(const CaptionRect: TRect): TRect;
  const
    cSideBuffer = 4;
  var
    CloseSize: Integer;
  begin
    CloseSize := CalcButtonSize(CaptionRect);
    if Self.DockCaptionOrientation = dcoHorizontal then
    begin
      Result.Left := CaptionRect.Right - CloseSize - cSideBuffer;
      Result.Top := CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) - CloseSize) div 2;
    end
    else
    begin
      Result.Left := CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) - CloseSize) div 2;
      Result.Top := CaptionRect.Top + 2 * cSideBuffer;
    end;
    Result.Right := Result.Left + CloseSize;
    Result.Bottom := Result.Top + CloseSize;
  end;

  function GetPinRect(const CaptionRect: TRect): TRect;
  const
    cSideBuffer = 4;
  var
    PinSize: Integer;
  begin
    PinSize := CalcButtonSize(CaptionRect);
    if Self.DockCaptionOrientation = dcoHorizontal then
    begin
      Result.Left := CaptionRect.Right - 2*PinSize - 2*cSideBuffer;
      Result.Top := CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) - PinSize) div 2;
    end
    else
    begin
      Result.Left := CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) - PinSize) div 2;
      Result.Top := CaptionRect.Top + 2*cSideBuffer + 2*PinSize;
    end;
    Result.Right := Result.Left + PinSize + 2;
    Result.Bottom := Result.Top + PinSize;
  end;

var
  ShouldDrawClose: Boolean;
  CloseRect, PinRect: TRect;
  LPngImage : TPngImage;
  LStartColor, LEndColor : TColor;
begin
  Canvas.Font.Color :=  DockerFontColor;
   //check the orientation of the dock caption
  if Self.DockCaptionOrientation = dcoHorizontal then
  begin
    Canvas.Pen.Width := 1;
    //set the color for the border of the caption bar
    Canvas.Pen.Color := DockerBorderColor;

    CaptionRect.Top := CaptionRect.Top + 1;
    //set the colors for the captin bar background
    if State.Focused then
    begin
      LStartColor := DockerStartEnabledColor;
      LEndColor   := DockerEndEnabledColor;
    end
    else
    begin
      LStartColor := DockerStartDisabledColor;
      LEndColor   := DockerEndDisabledColor;
    end;

    //draw the caption bar using a gradient
    GradientFillCanvas(Canvas, LStartColor, LEndColor, Rect(CaptionRect.Left + 1, CaptionRect.Top + 1, CaptionRect.Right, CaptionRect.Bottom), gdVertical);

    //draw the border of the caption bar
    Canvas.Pen.Color := DockerBorderColor;
    with CaptionRect do
      Canvas.Polyline([Point(Left + 2, Top), Point(Right - 2, Top), Point(Right, Top + 2),
        Point(Right, Bottom - 2), Point(Right - 2, Bottom), Point(Left + 2, Bottom), Point(Left, Bottom - 2), Point(Left, Top + 2), Point(Left + 3, Top)]);

    //draw the pin buttton
    CloseRect := GetCloseRect(CaptionRect);

    if Self.DockCaptionPinButton <> dcpbNone then
    begin
      PinRect := GetPinRect(CaptionRect);

      LPngImage:=TPNGImage.Create;
      try
        if Self.DockCaptionPinButton = dcpbUp then
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock_left')
        else
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock');

        Canvas.Draw(PinRect.Left, PinRect.Top, LPngImage);
      finally
        LPngImage.free;
      end;

      CaptionRect.Right := PinRect.Right - 2;
    end
    else
      CaptionRect.Right := CloseRect.Right - 2;

    CaptionRect.Left := CaptionRect.Left + 6;
    DrawIcon;
    ShouldDrawClose := CloseRect.Left >= CaptionRect.Left;

  end
  else
  begin
    Canvas.MoveTo(CaptionRect.Left + 1, CaptionRect.Top + 1);
    Canvas.LineTo(CaptionRect.Right - 1, CaptionRect.Top + 1);

    if State.Focused then
    begin
      LStartColor := DockerStartEnabledColor;
      LEndColor   := DockerEndEnabledColor;
    end
    else
    begin
      LStartColor := DockerStartDisabledColor;
      LEndColor   := DockerEndDisabledColor;
    end;

    GradientFillCanvas(Canvas, LStartColor, LEndColor,Rect(CaptionRect.Left, CaptionRect.Top + 2, CaptionRect.Right, CaptionRect.Bottom), gdVertical);

    Canvas.Pen.Color := DockerBorderColor;
    Canvas.MoveTo(CaptionRect.Left + 1, CaptionRect.Bottom);
    Canvas.LineTo(CaptionRect.Right - 1, CaptionRect.Bottom);

    Canvas.Font.Orientation := 900;
    CloseRect := GetCloseRect(CaptionRect);

    if Self.DockCaptionPinButton <> dcpbNone then
    begin
      PinRect := GetPinRect(CaptionRect);
      LPngImage:=TPNGImage.Create;
      try
        if Self.DockCaptionPinButton = dcpbUp then
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock_left')
        else
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock');

        Canvas.Draw(PinRect.Left, PinRect.Top, LPngImage);
      finally
        LPngImage.free;
      end;
      CaptionRect.Top := PinRect.Bottom + 2;
    end
    else
      CaptionRect.Top := CloseRect.Bottom + 2;

    ShouldDrawClose   := CaptionRect.Top < CaptionRect.Bottom;
    CaptionRect.Right := CaptionRect.Left + (CaptionRect.Bottom - CaptionRect.Top - 2);
    CaptionRect.Top   := CaptionRect.Top + Canvas.TextWidth(State.Caption) + 2;

    if CaptionRect.Top > CaptionRect.Bottom then
      CaptionRect.Top := CaptionRect.Bottom;
  end;

  Canvas.Brush.Style := bsClear;
  //draw the text of the caption bar
  if State.Caption <> '' then
  begin
    if State.Focused then
      Canvas.Font.Style := Canvas.Font.Style + [fsBold]
    else
      Canvas.Font.Style := Canvas.Font.Style - [fsBold];

   if ShouldDrawClose then
     CaptionRect.Right := CaptionRect.Right - (CloseRect.Right - CloseRect.Left) - 4;

    Canvas.TextRect(CaptionRect, State.Caption,  [tfEndEllipsis, tfVerticalCenter, tfSingleLine]);
  end;

  //draw the close buttton
  if ShouldDrawClose then
  begin
    LPngImage:=TPNGImage.Create;
    try
      LPngImage.LoadFromResourceName(HInstance, 'close_dock');
      Canvas.Draw(CloseRect.Left, CloseRect.Top, LPngImage);
    finally
      LPngImage.free;
    end;
  end;

  Exit(0);
end;


{$IF CompilerVersion>=27}
const
  sModernThemeDrawDockCaption = '@Moderntheme@TModernDockCaptionDrawer@DrawDockCaption$qqrxp20Vcl@Graphics@TCanvasrx18System@Types@TRectrx38Vcl@Captioneddocktree@TParentFormState';
{$ENDIF}

procedure RefreshForms;
var
  i : Integer;
begin
   for i := 0 to Screen.FormCount-1 do
    Screen.Forms[i].Invalidate;
end;

initialization
  Trampoline_TDockCaptionDrawer_DrawDockCaption  := InterceptCreate(@TDockCaptionDrawer.DrawDockCaption,   @CustomDrawDockCaption);
{$IF CompilerVersion>=27}
  ModernThemeModule := LoadLibrary('ModernTheme200.bpl');
  if ModernThemeModule<>0 then
  begin
   pModernThemeDrawDockCaption := GetProcAddress(ModernThemeModule, PChar(sModernThemeDrawDockCaption));
   if Assigned(pModernThemeDrawDockCaption) then
     Trampoline_ModernDockCaptionDrawer_DrawDockCaption:= InterceptCreate(pModernThemeDrawDockCaption, @CustomDrawDockCaption);
  end;
{$ENDIF}
  RefreshForms();
finalization
  if Assigned(Trampoline_TDockCaptionDrawer_DrawDockCaption) then
    InterceptRemove(@Trampoline_TDockCaptionDrawer_DrawDockCaption);
{$IF CompilerVersion>=27}
  if Assigned(Trampoline_ModernDockCaptionDrawer_DrawDockCaption) then
    InterceptRemove(@Trampoline_ModernDockCaptionDrawer_DrawDockCaption);
{$ENDIF}
  RefreshForms();

end.
