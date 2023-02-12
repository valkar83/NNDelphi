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
  LNeuralNetwork : TNeuralNetwork;
  LListeMatrix   : TList<TCoordDoubleMatrix>;
begin
  LListeMatrix := TList<TCoordDoubleMatrix>.Create;
  Charger.ChargerDonneesEnJson(LListeMatrix);
//  LList := TList<Integer>.Create;
//  LList.AddRange([1,2,3]);
//  LNeuralNetwork := TNeuralNetwork.Init(LList);
//  LList.free;
//  LNeuralNetwork.Free;
end;

end.
