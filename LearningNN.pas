unit LearningNN;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NeuralNetwork;

type
  TForm1 = class(TForm)
    procedure FormOnCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation
Uses
  CoordDouble,
  lireLesDonneesJSON,
  System.Generics.Collections;
{$R *.dfm}

procedure TForm1.FormOnCreate(Sender: TObject);
VAR
  LNbNeuronesParCouche : TList<Integer>;
  LNeuralNetwork       : TNeuralNetwork;
  LListeMatrix         : TList<TCoordDoubleMatrix>;

begin
  LListeMatrix := TList<TCoordDoubleMatrix>.Create;
  Charger.ChargerDonneesEnJson(LListeMatrix);
  LNbNeuronesParCouche := TList<Integer>.Create;
  LNbNeuronesParCouche.Add(784);
  LNbNeuronesParCouche.Add(30);
  LNbNeuronesParCouche.Add(10);
  LNeuralNetwork := TNeuralNetwork.Init(LNbNeuronesParCouche);
  LNeuralNetwork.StochasticGradientDescent(LListeMatrix, 30, 10, 3);
end;

end.
