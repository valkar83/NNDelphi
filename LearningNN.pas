unit LearningNN;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NeuralNetwork, CoordDouble,System.Generics.Collections;

type
  TForm1 = class(TForm)
    procedure FormOnCreate(Sender: TObject);
  private
    PROCEDURE MisANilMatrix(VAR AListeMatrix : TList<TCoordDoubleMatrix>);
  public

  end;
  var form1 : TForm1;

implementation
Uses
  lireLesDonneesJSON;
{$R *.dfm}

procedure TForm1.FormOnCreate(Sender: TObject);
VAR
  LNbNeuronesParCouche : TList<Integer>;
  LNeuralNetwork       : TNeuralNetwork;
  LListeMatrixTraining,
  LListeMatrixTest         : TList<TCoordDoubleMatrix>;

begin
  LListeMatrixTraining := TList<TCoordDoubleMatrix>.Create;
  LListeMatrixTest := TList<TCoordDoubleMatrix>.Create;
  Charger.ChargerDonneesEnJson(LListeMatrixTraining, 'traningData');
  Charger.ChargerDonneesEnJson(LListeMatrixTest, 'testData');
  LNbNeuronesParCouche := TList<Integer>.Create;
  LNbNeuronesParCouche.Add(784);
  LNbNeuronesParCouche.Add(30);
  LNbNeuronesParCouche.Add(10);
  LNeuralNetwork := TNeuralNetwork.Init(LNbNeuronesParCouche);
  LNeuralNetwork.StochasticGradientDescent(LListeMatrixTraining, 30, 10, 3,LListeMatrixTest);
  MisANilMatrix(LListeMatrixTraining);
  MisANilMatrix(LListeMatrixTest);
end;

procedure TForm1.MisANilMatrix(var AListeMatrix: TList<TCoordDoubleMatrix>);
VAR LI : Integer;
begin
  FOR LI := 0 TO (AListeMatrix.COunt - 1) DO
    TCoordDoubleMatrix(AListeMatrix[LI]).MisANilMatrix;
  FreeAndNil(AListeMatrix);
end;

end.
