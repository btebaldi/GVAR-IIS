#include <oxstd.oxh>
#import <packages/PcGive/pcgive_ects>

#include <.\Classes\ClasseCATS_Custom.ox>


/**
* Funcao de busca das variaveis, as regioes devem ser numeradas
* sequencialmente, mas podem ter um prefixo e um posfixo.
* ex: R_1_OIL
*
* @param    iQtdRegioes Quantidade de regioes no GVAR
* @param    sVarPrefix Prefixo do nome da variavel 
* @param    sVarPosfix Posfixo do nome da variavel
*
* @return   Colunas do banco de dados com as variaveis de todas as regioes. 
*/
GetRegionNames(const iQtdRegioes, const sVarPrefix, const sVarPosfix) {

    decl nCont, aNames;
    
    for (nCont = 1; nCont <= iQtdRegioes; ++nCont) {
        if (nCont == 1) {
            aNames = {sprint(sVarPrefix, nCont, sVarPosfix)};
        } else {
            aNames = aNames ~ {sprint(sVarPrefix, nCont, sVarPosfix)};
        }
    }
    return aNames;
}

/**
*
* @param    mRankMatrix
*
* @return    
*/
EstimateRank(const mRankMatrix){
	decl iRank;
    for(decl irow = 0; irow < rows(mRankMatrix); irow++) {
		iRank = mRankMatrix[0][irow][1];
		if(mRankMatrix[0][irow][6] > 0.05){
			break;
		} else {
			iRank = iRank + 1;
		}
	}
    return(iRank);
}


//GetBetaEstimative(const mBeta, const iRank){
//	println(mBeta);
//    decl ret;
//	if (iRank == 0){
//		ret = zeros(4, 1);
//	} else {
//		ret = mBeta[][0:iRank-1];
//	}
//	return ret;
//}
//
//SaveBetaEstimative(const spath, const mBeta, const iRank){
//	decl mbetaTransp = GetBetaEstimative(mBeta, iRank);
//	savemat(spath, mbetaTransp');
//}


main() {
    // Arquivo de configuracao
    #include "./Config.ox";


    println("Carregando dados de macrovariaveis");
    // decl mMacroData;
    // decl daBaseMacro = new Database();

    // daBaseMacro.Load(txDbaseMacroVariables);
    
    //print( "%c", daBaseMacro.GetAllNames(), "%cf", daBaseMacro.GetAll());
    // println(" Carregando dados das colunas: ", aMacroVarNames);
    
    // mMacroData = daBaseMacro.GetVar(aMacroVarNames);
    
    //print( "%c", aMacroVarNames, "%cf", mMacroData[0:9][]);
    // delete daBaseMacro;
    // println("Macrovariaveis carregadas");

    println("Carregando matrix de pessos W");
    decl mW;

    mW = loadmat(sprint(txMatPathW_Matrix, "CONN.mat"));

    println("*** Iniciando estimacao dos modelos *** \n");
    
    // iCont : Contador da regiao atual
    decl iCont;

    for (iCont = 1; iCont <= 1; ++iCont) {

        // FOR DEBUG ONLY
       
		// print Headder
        println("\n\n*****************************************");
        println("             Regiao ", iCont);
        println("*****************************************\n\n");
        
        // Inicio um nomo objeto do tipo database
        decl modelDatabase = new Database();
        
        // Matriz de selecao dos vetores de cointegracao
        decl mRankMatrix; 
        
         // Rank selecionado
        decl iRank;
        
        println("\nCarregando base de dados para regiao ", iCont);
        modelDatabase.Load(txDbase);

        println("\tPeriodo da base de dados");
        println("\tData inicial: ", modelDatabase.GetYear1(), "-", modelDatabase.GetPeriod1());
        println("\tData final: ", modelDatabase.GetYear2(), "-", modelDatabase.GetPeriod2());

        // As Variaveis Star sao uma combinacao linear das variaveis esternas.
        println("(1) Iniciando construcao das variaveis star para a regiao ", iCont);

        // mData: Matrix com as variaveis
        // beta: vetores de cointegracao.
        decl mData, mBeta;
        mBeta =0;

        println("\tAdicionando variavel star da regiao ", iCont);
        decl iContador;
        for (iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            
            mData = modelDatabase.GetVar(GetRegionNames(iQtdRegioes, "R_", sprint("_", aVarDependenteNames[iContador])));
           	modelDatabase.Append(mData * mW[][iCont - 1], sprint("star_", aVarDependenteNames[iContador]));
        }
        println("\tConcluido construcao das variaveis star para a regiao ", iCont);

        println("(2) Iniciando construcao da variavel Delta para a regiao ", iCont);
        for(iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            // Adiciona a variavel em primeira Diferenca
            mData =	modelDatabase.GetVar(sprint("R_", iCont, "_", aVarDependenteNames[iContador]));
            modelDatabase.Append(diff(mData), sprint("D_R", iCont, "_", aVarDependenteNames[iContador]));

            // Adiciona a variavel Star em primeira diferenca
            mData =	modelDatabase.GetVar(sprint("star_", aVarDependenteNames[iContador]));
            modelDatabase.Append(diff(mData), sprint("D_star", "_", aVarDependenteNames[iContador]));
        }
        println("\tConcluido construcao da variavel Delta para a regiao ", iCont);

// modelDatabase.SaveIn7(sprint("DEBUG_", iCont, "_Fulldatabase"));  

        println("\tIniciando determinacao do vetor de cointegracao (beta) a regiao ", iCont);
        // Inicio um objeto do CATS (Cointegration)
    	decl modelCats = new GVAR_CATS();
	
		// modelCats.Resample(12, 1995, 1);

        for(iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            // Adiciona a variavel em primeira Diferenca
            mData =	modelDatabase.GetVar(sprint("R_", iCont, "_", aVarDependenteNames[iContador]));
            modelCats.Append(mData, sprint("R", iCont, "_", aVarDependenteNames[iContador]));
        }

        for(iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            // Adiciona a variavel Star em primeira diferenca
            mData =	modelDatabase.GetVar(sprint("star_", aVarDependenteNames[iContador]));
            modelCats.Append(mData, sprint("star", "_", aVarDependenteNames[iContador]));
        }

        modelCats.SaveIn7(sprint("R_", iCont, "_database"));

    	// Adiciona as variaveis X como exogenas
        for(iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            println("append: ",sprint("R", iCont, "_", aVarDependenteNames[iContador]));
            modelCats.Select("Y", {sprint("R", iCont, "_", aVarDependenteNames[iContador]), 0, 0});
        }
        for(iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            println("append: ",sprint("star", "_", aVarDependenteNames[iContador]));
            modelCats.Select("Y", {sprint("star", "_", aVarDependenteNames[iContador]), 0, 0});
        }
        modelCats.Lags(iQtdLags, iQtdLags, iQtdLags);

	    // Rank inicial (mudar para a quantidade de variaveis.)
	    modelCats.I1Rank(6);

        // Tipo de cointegracao CIMEAN: Constante no espaço de cointegracao.
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
        modelCats.Estimate();

// modelCats.PrintI1Rank();
        // Estima vetores do cointegração por bootstrap
       	mRankMatrix = modelCats.BootstrapRankTest();

        // Escolhe o Rank 
        iRank = EstimateRank(mRankMatrix);
        println(EstimateRank(mRankMatrix));

// if(iRank == 0){
// println("RANK ZERO DETECTADO, MUDANDO PARA RANK=1");
// continue;
// 		iRank=1;
// }

        mBeta = modelCats.GetBeta();
        // println(mRankMatrix);

        // Se o rank for maior que dois Automaticamente teremos de modelar as variaveis no modelo dominante
//         if(iRank > 2){
//              salva a estimacao do beta PARA AS REGIOES COM MAIS DE 3VETORES DE COINTEGRACAO
//             modelCats.SaveBetaEstimative(sprint(txCoIntMatPath, sprint("Dominant3_CoInt_R", iCont, ".mat")), mBeta, iRank);
//         } else {
// 			 Restima o modelo com os dados de cointegracao.
// println("RANK TOTAL: ",iRank);
//             modelCats.I1Rank(iRank);
//             modelCats.Estimate();
//             modelCats.BootstrapRankTest();

//             modelCats.SetPrint(TRUE);
            // Estima a exogeniedade fraca
// 			modelCats.Restrict({"[beta]","[alpha]","* * 0 0","* * 0 0"});
//         	modelCats.BootstrapRestrictions();
// println("TESTE REGIAO ", iCont, " (hail mary)");
// 			modelCats.Restrict({"[beta]","* * * *","0 0 * *","[alpha]","* * 0 0","0 0 * *"});
//         	modelCats.BootstrapRestrictions();
            // println("a", a[0]);
            // println("a", a[1]);
            // println("a", a[2]);
            // println("a", a[3]);
            // println("a", modelCats.GetAlpha());

//             modelCats.SaveBetaEstimative(sprint(txCoIntMatPath, sprint("Weak2_CoInt_R", iCont, ".mat")), mBeta, iRank);
//         }

        // Guarda o valor do Beta
        // mBeta = model.GetBeta();

        delete modelCats;
        delete modelDatabase;

        // Apago variaveis que nao serao mais utilizadas
        // delete mData, mBeta, asX;
    } // for (iCont = 1; iCont <= iQtdRegioes; ++iCont)

    delete mW;
    println("*** Fim da estimacao dos modelos regionais *** \n");
} // End of main()
