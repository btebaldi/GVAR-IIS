// Import the Ox library
#include <oxstd.oxh>

// required to use Database class
#import <database>

// List of helper functions designed by Bruno Tebaldi
#include "./my_help.ox"

main() {
    // Arquivo de configuracao
    #include "./Config.ini"
    
    /* ********* CARREGANDO MATRIX DE PESOS ********* */
    println("Carregando matrix de pessos W");
    decl mW;
    mW = loadmat(txMatrixDePesos);
    
    // Inicio um nomo objeto do tipo Database
    println("Iniciando um nomo objeto do tipo Database.");
    decl my_database = new Database();

    println("\nCarregando base de dados.");
    my_database.Load(txDbase);

    // Carrega a informação de qual o rank de cada região
    for (decl iCont = 1; iCont <= iQtdRegioes; ++iCont) {
        println("\n\n*****************************************");
        println("             Regiao ", iCont);
        println("*****************************************\n\n");

        /* ********* CONTRUCAO DAS VARIAVEIS ESTRELA (EXTERNAS) ********* */
        println("(1) Iniciando construcao das variaveis star para a regiao ", iCont);

        // Vartiavel que contem as informacoes das regioes
        decl mData;

        // As Variaveis Star sao uma combinacao linear das variaveis esternas.
        for (decl iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            println("(2.",iContador,") Adicionando variavel star da regiao ", iCont, " (", aVarDependenteNames[iContador], ")");
            mData = my_database.GetVar(GetRegionNames(iQtdRegioes, "R_", sprint("_", aVarDependenteNames[iContador])));
            my_database.Append(mData * mW[][iCont - 1], sprint("R_", iCont, "_", aVarDependenteNames[iContador], "_star"));
        }
        
        println("(3) Concluido construcao das variaveis star para a regiao ", iCont);
        delete mData;
    }

    // Save the database
    my_database.Save(txDbaseStar);
    
    // Clean up
    delete my_database;
}
