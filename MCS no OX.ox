#include <oxstd.oxh>
#import <packages/Mulcom/Mulcom>

main()
{

	// Quantidade de Regioes
	decl iQtdRegioes;
	iQtdRegioes = 110;

	// Lista de Combustiveis
	decl lstCombstivel;
	lstCombstivel = {"E", "D", "G"};

	// Endereco da base de dados
	decl basePath;
	basePath = "C:\\Users\\bteba\\Documents\\GitHub\\Tese_Cap2\\Export_erros_to_Marcal.csv";
	
	decl iContador;
	for (iContador = 0; iContador < columns(lstCombstivel); ++iContador) {
		
		println("\tFazendo modelos do tipo de combustivel: ", lstCombstivel[iContador]);

		decl tipoCombstivel;
		tipoCombstivel = lstCombstivel[iContador];

		decl numRegiao;
		for (numRegiao = 1; numRegiao <= iQtdRegioes; ++numRegiao) {

			println("\tIniciando modelo variavel ", numRegiao, " com o combustivel ", tipoCombstivel);

			//--- Ox code for MulCom( 3)
			decl model = new MulCom();

			// Carregando base de dados
			model.Load(basePath);
			
			model.Deterministic(-1);

			model.Select("O", {"OBS", 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_VECM_R" ,  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_VECM2_R" ,  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M1_R" ,  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M12_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M13_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M14_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M15_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M16_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M17_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M18_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M21_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M22_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M23_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M24_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M25_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M26_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M27_R",  numRegiao) , 0, 0});
			model.Select("P", { sprint( tipoCombstivel, "_M28_R",  numRegiao) , 0, 0});
			model.SetSelSample(1, 1, 122, 1);
			model.Do_MCS(0.1,2,10000);
			model.Set_TestStatMCS("Range");
			model.Set_Loss("mad");
			model.DoEstimation();

			delete model;
		}
		
		delete tipoCombstivel;
	}
}
