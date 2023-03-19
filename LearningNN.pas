unit LearningNN;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NeuralNetwork, CoordDouble,System.Generics.Collections,
  Vcl.ExtDlgs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    dlgOpenPic1: TOpenPictureDialog;
    imgChiffreADeviner: TImage;
    lbl1: TLabel;
    btn1: TButton;
    lbl2: TLabel;
    procedure FormOnCreate(Sender: TObject);
    procedure OuvrirImageClick(Sender: TObject);
    procedure FormOnDestroy(Sender: TObject);
    procedure BtnChiffreIdentifieClick(Sender: TObject);
  private
     FFChiffreDevine : Integer;
     FNeuralNetwork       : TNeuralNetwork;
    PROCEDURE MisANilMatrix(VAR AListeMatrix : TList<TCoordDoubleMatrix>);
  public
  end;
  var form1 : TForm1;

implementation
Uses
  Matrix,
  System.UIConsts,
  System.Math,
  lireLesDonneesJSON;
{$R *.dfm}

procedure TForm1.BtnChiffreIdentifieClick(Sender: TObject);
begin
  IF FFChiffreDevine <> -1 THEN lbl2.Caption := IntToStr(FFChiffreDevine);
end;

procedure TForm1.FormOnCreate(Sender: TObject);
VAR
  LNbNeuronesParCouche : TList<Integer>;
  LListeMatrixTraining,
  LListeMatrixTest         : TList<TCoordDoubleMatrix>;
begin
  FFChiffreDevine := -1;
  LListeMatrixTraining := TList<TCoordDoubleMatrix>.Create;
  LListeMatrixTest := TList<TCoordDoubleMatrix>.Create;
  Charger.ChargerDonneesEnJson(LListeMatrixTraining, 'traningData');
  Charger.ChargerDonneesEnJson(LListeMatrixTest, 'testData');
  LNbNeuronesParCouche := TList<Integer>.Create;
  LNbNeuronesParCouche.Add(784);
  LNbNeuronesParCouche.Add(30);
  LNbNeuronesParCouche.Add(10);
  FNeuralNetwork := TNeuralNetwork.Init(LNbNeuronesParCouche);
  FNeuralNetwork.StochasticGradientDescent(LListeMatrixTraining, 30, 10, 3, LListeMatrixTest);
  MisANilMatrix(LListeMatrixTraining);
  MisANilMatrix(LListeMatrixTest);
end;

procedure TForm1.FormOnDestroy(Sender: TObject);
begin
  FNeuralNetwork.Free;
end;

procedure TForm1.MisANilMatrix(var AListeMatrix: TList<TCoordDoubleMatrix>);
VAR LI : Integer;
begin
  FOR LI := 0 TO (AListeMatrix.COunt - 1) DO
    TCoordDoubleMatrix(AListeMatrix[LI]).MisANilMatrix;
  FreeAndNil(AListeMatrix);
end;

procedure TForm1.OuvrirImageClick(Sender: TObject);
VAR
  LI, LJ : Integer;
  LTab : TArray<Double>;
  LColor : TColor;
  LH, LS, LL : Single;
  LMatrixChiffreEntree: IMatrix;
begin
  LTab := [];
  IF dlgOpenPic1.Execute(Self.Handle) THEN
  BEGIN
    imgChiffreADeviner.Picture.LoadFromFile(dlgOpenPic1.FileName);
    FOR LI := 0 TO (imgChiffreADeviner.Width - 1) DO
    BEGIN
      FOR LJ := 0 TO (imgChiffreADeviner.Height - 1) DO
      BEGIN
        LColor := imgChiffreADeviner.Canvas.Pixels[LJ, LI];
        RGBtoHSL(LColor, LH, LS, LL);
        LL := 1 - LL;
        LTab := LTab + [Double(LL)];
      END;
    END;
    LMatrixChiffreEntree := TDoubleMatrix.Create(LTab, 1, imgChiffreADeviner.Width * imgChiffreADeviner.Height);
    FNeuralNetwork.feedforward(LMatrixChiffreEntree);
    FFChiffreDevine := FNeuralNetwork.RenvoyerIndiceMax(LMatrixChiffreEntree);
  END;
end;

end.
