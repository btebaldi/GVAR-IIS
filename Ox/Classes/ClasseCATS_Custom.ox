#include <oxstd.oxh>
#import <packages/CATS/CATS>

class GVAR_CATS : CATS
{
	GVAR_CATS(); // Construtor
	GetBetaEstimative(const mBeta, const iRank);
	SaveBetaEstimative(const spath, const mBeta, const iRank);
	Rank();
};

GVAR_CATS::GVAR_CATS()
{
	CATS();
	println("Inicializando a Classe GVAR_CATS!");
}

GVAR_CATS::GetBetaEstimative(const mBeta, const iRank){
	// println(mBeta);
    decl ret;
	if (iRank == 0){
		ret = zeros(4, 1);
	} else {
		ret = mBeta[][0:iRank-1];
	}
	return ret;
}

GVAR_CATS::SaveBetaEstimative(const spath, const mBeta, const iRank){
	decl mbetaTransp = GetBetaEstimative(mBeta, iRank);
	savemat(spath, mbetaTransp');
}

GVAR_CATS::Rank(){
	return m_iR;
}


