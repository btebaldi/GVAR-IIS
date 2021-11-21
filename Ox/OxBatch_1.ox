#include <oxstd.oxh>
#import <packages/CATS/CATS>

main()
{
	//--- Ox code for CATS( 3)
	decl model = new CATS();

	model.Load("K:\\Github\\Tes_Cap2\\Ox\\R_1_database.in7");
	model.Select("Y", {"R_1_ETANOL_HIDRATADO", 0, 5});
	model.Select("Y", {"R_1_OLEO_DIESEL", 0, 5});
	model.Select("Y", {"R_1_GASOLINA_COMUM", 0, 5});
	model.Select("Y", {"star_ETANOL_HIDRATADO", 0, 0});
	model.Select("Y", {"star_OLEO_DIESEL", 0, 0});
	model.Select("Y", {"star_GASOLINA_COMUM", 0, 0});
	model.Select("X", {"brent", 0, 5});
//	model.Select("U", {"Constant", 0, 0});

	//model.Lags(5);
	model.I1Rank(6);
	model.Trend("DRIFT");
	model.SetSelSample(6, 1, 891, 1);
	model.SetMethod("RRR");
	model.SetPrint(FALSE);

	model.Estimate();

	model.ShortRun();
	model.TestSummary();

	//model.ACTest_LM(model.m_mResidual);
	//	ACTest_LB ( const mResid , iOrder = 0 )

	delete model;
}
