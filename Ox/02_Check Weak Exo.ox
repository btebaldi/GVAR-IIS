#include <oxstd.oxh>
#import <packages/PcGive/pcgive_ects>

// Helper Cats class designed by Bruno Tebaldi
#include <.\Classes\ClasseCATS_Custom.ox>

// List of helper functions designed by Bruno Tebaldi
#include "./my_help.ox"




/**
*
* @param    mRankMatrix
*
* @return    
*/
EstimateRank(const mRankMatrix, const col){

    println("RANK TABLE");
    println(mRankMatrix);

	decl iRank;
    for(decl irow = 0; irow < rows(mRankMatrix); irow++) {
        iRank = mRankMatrix[irow][1];
        // println("myiRank: ", iRank);
        // println("myValue: ", mRankMatrix[irow][col]);
		if(mRankMatrix[irow][col] > 0.05){
			break;
		} else {
			iRank = iRank + 1;
		}
	}
    return(iRank);
}




main() {
    // Arquivo de configuracao
    #include "./Config.ini";


    // println("Carregando dados de macrovariaveis");
    // decl mMacroData;
    // decl daBaseMacro = new Database();

    // daBaseMacro.Load(txDbaseMacroVariables);
    
    //print( "%c", daBaseMacro.GetAllNames(), "%cf", daBaseMacro.GetAll());
    // println(" Carregando dados das colunas: ", aMacroVarNames);
    
    // mMacroData = daBaseMacro.GetVar(aMacroVarNames);
    
    //print( "%c", aMacroVarNames, "%cf", mMacroData[0:9][]);
    // delete daBaseMacro;
    // println("Macrovariaveis carregadas");

    
    // DELETE
    // println("Carregando matrix de pessos W");
    // decl mW;
    // mW = loadmat(sprint(txMatPathW_Matrix, "PIB_PC_1.mat"));

    println("*** Iniciando estimacao dos modelos *** \n");
       
        // Inicio um nomo objeto do tipo database
        decl modelDatabase = new Database();
        modelDatabase.Load(txDbaseStar);
    
    for (decl iCont = 1; iCont <= iQtdRegioes; ++iCont) {

        // FOR DEBUG ONLY
        // zero cointegracao:  13,52,77, 99, 114, 115
		// if( any(<12,16> .== iCont)){
		// println("SKIP: Regiao ", iCont);
        //      continue;
        // }

		// print Headder
        println("\n\n*****************************************");
        println("             Regiao ", iCont);
        println("*****************************************\n\n");
        

        println("\tPeriodo da base de dados");
        println("\tData inicial: ", modelDatabase.GetYear1(), "-", modelDatabase.GetPeriod1());
        println("\tData final: ", modelDatabase.GetYear2(), "-", modelDatabase.GetPeriod2());

        // Matrix que ira guardar o valor dos dados
        decl mData;

        println("(2) Iniciando construcao da variavel Delta para a regiao ", iCont);
        for(decl iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            // Adiciona a variavel em primeira Diferenca
            mData =	modelDatabase.GetVar(sprint("R_", iCont, "_", aVarDependenteNames[iContador]));
            modelDatabase.Append(diff(mData), sprint("D_R_", iCont, "_", aVarDependenteNames[iContador]));

            // Adiciona a variavel Star em primeira diferenca
            mData =	modelDatabase.GetVar(sprint("R_", iCont, "_", aVarDependenteNames[iContador], "_star"));
            modelDatabase.Append(diff(mData), sprint("D_R_", iCont, "_", aVarDependenteNames[iContador], "_star") );
        }
        println("\tConcluido construcao da variavel Delta para a regiao ", iCont);

        println("\tIniciando determinacao do vetor de cointegracao (beta) a regiao ", iCont);
        // Inicio um objeto do CATS (Cointegration)
    	decl modelCats = new GVAR_CATS();

        /* Adiciona a variavel em em Nivel */
        for(decl iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            mData =	modelDatabase.GetVar(sprint("R_", iCont, "_", aVarDependenteNames[iContador]));
            modelCats.Append(mData, sprint("R_", iCont, "_", aVarDependenteNames[iContador]));
        }

        /* Adiciona a variavel Star em Nivel */
        for(decl iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            mData =	modelDatabase.GetVar( sprint("R_", iCont, "_", aVarDependenteNames[iContador], "_star") );
            modelCats.Append(mData, sprint("R_", iCont, "_", aVarDependenteNames[iContador], "_star") );
        }

        /* Adiciona a variavel macroeconomicas (exogenas) (em Nivel) */
        for(decl iContador = 0; iContador < columns(aMacroVarNames); ++iContador) {
            mData =	modelDatabase.GetVar(sprint(aMacroVarNames[iContador]));
            modelCats.Append(mData, sprint(aMacroVarNames[iContador]));
        }

        // Adiciona a variaveis dummies
        // for(iContador = 1; iContador < 12; ++iContador) {
        //     mData =	modelDatabase.GetVar(sprint("M", iContador));
        //     modelCats.Append(mData, sprint("M", iContador));
        // }
        

    	// Adiciona as variaveis da regiao como endogenas
//        for(decl iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
//            println("append: ", sprint("R_", iCont, "_", aVarDependenteNames[iContador]));
//            modelCats.Select("Y", {sprint("R_", iCont, "_", aVarDependenteNames[iContador]), 0, 0});
//        }
		
        for(decl iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            println("append: ", sprint("R_", iCont, "_", aVarDependenteNames[iContador], "_star") );
            modelCats.Select("Y", {sprint("R_", iCont, "_", aVarDependenteNames[iContador], "_star"), 0, 0});
        }
//
		// Adiciona as variaveis Macro como exogenas
         for(decl iContador = 0; iContador < columns(aMacroVarNames); ++iContador) {
             println("append: ", sprint(aMacroVarNames[iContador]));
             modelCats.Select("X", {sprint(aMacroVarNames[iContador]), 0, 0});
         }

//         for(iContador = 1; iContador < 12; ++iContador) {
//             println("append: ", sprint("M", iContador));
//             modelCats.Select("X", {sprint("M", iContador), 0, 0});
//         }

        modelCats.Lags(iQtdLags, iQtdLags, iQtdLags);

	    // Rank inicial (mudar para a quantidade de variaveis.)
	    modelCats.I1Rank(4);

        // Tipo de cointegracao CIMEAN: Constante no espaÃ§o de cointegracao.
        // mode	string: one of "NONE","CIMEAN","DRIFT","CIDRIFT".
        // Equivalently, use the strings "H_z","H_c","H_lc","H_l", or the predefined constants CATS::NONE, CATS::CIMEAN, CATS::DRIFT, CATS::CIDRIFT.
        modelCats.Trend("DRIFT");

        // Inclui seasonal centradas
        modelCats.Seasonals(1);

        // fixa a amostra
        // modelCats.SetSelSample(1995, 1, 1998, 12);
        
        // tipo de metodo RRR: Reduced Rank Regression
        modelCats.SetMethod("RRR");

        // set print to false
        modelCats.SetPrint(TRUE);

        // Estima o modelo.
        // modelCats.SaveIn7(sprint("R_", iCont, "_database"));
        modelCats.Estimate();

        modelCats.TestSummary();

        // Escolhe o Rank 
        decl mRankMatrix = modelCats.I1RankTable();
        decl iRank = EstimateRank(mRankMatrix, 6);
        println("RANK ESTIMADO NORMAL: ", iRank);
        iRank = EstimateRank(mRankMatrix, 7);
        println("RANK ESTIMADO NORMAL_BARLET: ", iRank);

        // modelCats.PrintI1Rank();
        // Estima vetores do cointegraÃ§Ã£o por bootstrap
       	// if(any(<94, 95, 97, 98, 105, 107, 108, 109, 110> .== iCont)){
            mRankMatrix = modelCats.BootstrapRankTest();
            iRank = EstimateRank(mRankMatrix[0], 7);
            println("RANK ESTIMADO BOOSTRAP: ", iRank);
        // }

        if(iRank == 0){
            println("RANK ZERO DETECTADO, MUDANDO PARA RANK=1");
            iRank=1;
        }

        decl mBeta;
        mBeta = modelCats.GetBeta();
        // println(mRankMatrix);

		if(1==2){
        // Se o rank for maior que dois Automaticamente teremos de modelar as variaveis no modelo dominante
        if(iRank > 6){
            //  salva a estimacao do beta PARA AS REGIOES COM MAIS DE 3 VETORES DE COINTEGRACAO
            modelCats.SaveBetaEstimative(sprint(txCoIntMatPath, sprint("Dominant3_CoInt_R", iCont, ".mat")), mBeta, iRank);
        } else {
			//  Restima o modelo com os dados de cointegracao.
            println("RANK TOTAL: ",iRank);
            modelCats.I1Rank(iRank);
            modelCats.Estimate();
            //modelCats.BootstrapRankTest();

            modelCats.SetPrint(TRUE);
            
            // Estima a exogeniedade fraca
            println("TESTE A EXOGENIEDADE FRACA: Regiao ", iCont);
			if(iRank == 1){
                modelCats.Restrict({"[beta]","[alpha]","* * 0 0"});
            } else if(iRank == 2){
                modelCats.Restrict({"[beta]","[alpha]","* * 0 0", "* * 0 0"});
            } else if(iRank == 3) {
                modelCats.Restrict({"[beta]","[alpha]","* * 0 0", "* * 0 0", "* * 0 0"});
            } else if(iRank == 4) {
                modelCats.Restrict({"[beta]","[alpha]","* * 0 0", "* * 0 0", "* * 0 0", "* * 0 0"});
            } else if(iRank == 5) {
                modelCats.Restrict({"[beta]","[alpha]","* * 0 0", "* * 0 0", "* * 0 0", "* * 0 0", "* * 0 0"});
            } else {
                println("DEU MERDA!");
            }
        	modelCats.BootstrapRestrictions();

            // println("TESTE A SEPARABILIDADE ", iCont, " (hail mary)");
			// modelCats.Restrict({"[beta]","* * * *", "0 0 * *","[alpha]","* * 0 0","0 0 * *"});
        	// modelCats.BootstrapRestrictions();
            // println("a", modelCats.GetAlpha());

            modelCats.SaveBetaEstimative(sprint(txCoIntMatPath, sprint("Weak2_CoInt_R", iCont, ".mat")), mBeta, iRank);
        }
		}
        // Guarda o valor do Beta
        // mBeta = model.GetBeta();

        delete modelCats;

        // Apago variaveis que nao serao mais utilizadas
        delete mData, mBeta;
    } // for (iCont = 1; iCont <= iQtdRegioes; ++iCont)

    delete modelDatabase;

    // delete mW;
    println("*** Fim da estimacao dos modelos regionais *** \n");
} // End of main()
