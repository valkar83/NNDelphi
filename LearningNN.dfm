object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 191
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormOnCreate
  OnDestroy = FormOnDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object imgChiffreADeviner: TImage
    Left = 208
    Top = 66
    Width = 28
    Height = 28
    Cursor = crHandPoint
    OnClick = OuvrirImageClick
  end
  object lbl1: TLabel
    Left = 8
    Top = 24
    Width = 424
    Height = 26
    Cursor = crHandPoint
    Caption = 
      'L'#39'image doit avoir cette dimension : 28x28px. Et le chiffre doit' +
      ' '#234'tre positionn'#233' au centre.'#13#10
    OnClick = OuvrirImageClick
  end
  object lbl2: TLabel
    Left = 286
    Top = 105
    Width = 13
    Height = 20
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btn1: TButton
    Left = 167
    Top = 100
    Width = 113
    Height = 25
    Caption = 'Identifie le chiffre'
    TabOrder = 0
    OnClick = BtnChiffreIdentifieClick
  end
  object dlgOpenPic1: TOpenPictureDialog
    Left = 248
    Top = 64
  end
end
