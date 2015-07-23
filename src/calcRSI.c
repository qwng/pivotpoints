void calcRSI(double gain[], double loss[], double rsi[], int *period, int *length){
	int i, j;
	double sumGain, sumLoss;

	for(i = (*period - 1); i != *length; i++){
		sumGain = 0, sumLoss = 0;
		for(j = (i + 1 - *period); j != (i + 1); j++){
			sumGain += gain[j];
			sumLoss += loss[j]; 
		}
	if(sumLoss != 0){
		rsi[i] = 100 - 100 / (1 + sumGain / (-sumLoss));
	} 
	else{
		rsi[i] = 0;
	}
	}
}