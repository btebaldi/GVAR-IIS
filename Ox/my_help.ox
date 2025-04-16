
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
    // println("iQtdRegioes: ", iQtdRegioes);
    // println("sVarPrefix: ", sVarPrefix);
    // println("sVarPosfix: ", sVarPosfix);
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
