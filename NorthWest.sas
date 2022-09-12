
proc iml;

cost = {6 4 1,
		3 8 7,
		4 4 2};
demand = {20 95 35};
supply = {50,
		  40,
		  60};
		
nrows = nrow(cost);
ncols = ncol(cost);


columns=1;
rows=1;
currentDemand = 1;
currentSupply = 1;
TotalCost = 0;
usedCells = 0;
selectedCosts = cost;
u1 = 0;

basicCost = j(nrows,ncols,99);
DemandCost = j(nrows,ncols,0);
nonBasicCost = j(nrows,ncols,0);


   do while(columns<=ncols);
   	  currentDemand = demand[1,columns];
   	  currentSupply = supply[rows,1];
   
   	  do while(currentDemand>0);

      	if supply[rows,1] >= currentDemand then do;
   			supply[rows,1] = supply[rows,1] - currentDemand;
   			DemandCost[rows,columns] = currentDemand;
   			multi = cost[rows,columns] * currentDemand;
   			string = cats('Row : ', rows , ' || Column : ' , columns , ' || Cost : ',multi);
			print string;
			/*Basic Variables*/
			if rows = 1 then do;
			basicCost[rows,columns] = cost[rows,columns];
			lastColumn = basicCost[rows,columns];
			end;
			else do;
			basicCost[rows,columns] = cost[rows,columns] - lastColumn;
			lastColumn = basicCost[rows,columns];
			end;
			/*End Basic Variables*/
			selectedCosts[rows,columns] = 0;
			usedCells = usedCells + 1;
   			TotalCost = TotalCost + multi;
			demand[1,columns] = 0;
			currentDemand = 0;
			columns = columns+1;
   			end;
   			else do;
   			currentDemand = currentDemand - supply[rows,1];
   			DemandCost[rows,columns] = supply[rows,1];
   			multi = cost[rows,columns] * supply[rows,1];
   			string = cats('Row : ', rows , ' || Column : ' , columns , ' || Cost : ',multi);
			print string;
			/*Basic Variables*/
			if rows = 1 then do;
			basicCost[rows,columns] = cost[rows,columns];
			lastColumn = basicCost[rows,columns];
			end;
			else do;
			basicCost[rows,columns] = cost[rows,columns] - lastColumn;
			lastColumn = basicCost[rows,columns];
			end;
			/*End Basic Variables*/
			selectedCosts[rows,columns] = 0;
			usedCells = usedCells + 1;
   			TotalCost = TotalCost + multi;
   			demand[1,columns] = currentDemand;
   			supply[rows,1] = 0;
   			rows = rows+1;
   			currentSupply = supply[rows,1];
   			end;
  		
      end;
      
   end;


print TotalCost;


u = 0;
v = 0;
/* Step Stone */
CheckOptimality = (nrows + ncols)-1;
	if CheckOptimality ^= usedCells then do;
	Warnning = cat('The Optimality is false because used cells not equal (row+columns) - 1: ', usedCells , ' != ' , CheckOptimality);
	print(Warnning);
	end; 
	else do;
	/*/----------------*/
	print(selectedCosts);
	rowCost = 1;
	columnCost = 1;
	
   	do while(rowCost<=nrows);
   	
   		do while(columnCost<=ncols);
   		
   		if selectedCosts[rowCost,columnCost] ^= 0 then do;
   		UnUsedCost = selectedCosts[rowCost,columnCost];
   		rowStep = rowCost;
   		columnStep = columnCost;
   		print UnUsedCost;
   		
   		
   		/*Non Basic Cost*/
   		if rowStep = 1 then do;
   			do vLoop = 1 to nrows;
   				if basicCost[vLoop,columnStep] ^= 99 then do;
   				nonBasicCost[rowStep,columnStep] = basicCost[vLoop,columnStep] - cost[rowStep,columnStep];
   				vLoop = nrows + 1;
   				end;
			end;
   		
   		end;
   		else do;
   			do vLoop = 1 to nrows;
   				if basicCost[vLoop,columnStep] ^= 99 then do;
   				v = basicCost[vLoop,columnStep];
   				vLoop = nrows + 1;
   				end;
			end;
			do uLoop = 1 to ncols;
   				if basicCost[rowStep,uLoop] ^= 99 then do;
   				u = basicCost[rowStep,uLoop];
   				uLoop = ncols + 1;
   				end;
			end;

   		nonBasicCost[rowStep,columnStep] = (u + v) - cost[rowStep,columnStep];
   		
	
   		end;
   		/*End Non Basic Cost*/
   			
   		columnCost = columnCost + 1;

   			end;
   			else do;
   			
   			columnCost = columnCost + 1;
   			end;
   			
   			end;
   		
   	columnCost = 1;
   	rowCost = rowCost + 1;
  		
    end;
      
	
	end;

print basicCost;
print nonBasicCost;
print DemandCost;



start detectRows(row,column,basicCost,StepCost,stepNum,num,nrows,ncols);
	DetectRow = 1;

	do while(DetectRow <= nrows);
   	if basicCost[DetectRow,column] ^= 99 & DetectRow ^= row then do;
		row = DetectRow;
		if MOD(num,2)=1 then do;
			stepNum[1,num] = StepCost[row,column];
		end;
		else do;
		stepNum[1,num] = StepCost[row,column];
		end;
		num = num + 1;
		DetectRow = 99;
		end;
		else do;
		DetectRow = DetectRow + 1;
		end;
	end;
   				

finish;


start detectColumns(row,column,basicCost,StepCost,stepNum,num,nrows,ncols);
	DetectColumn = 1;

	do while(DetectColumn <= ncols);
   	if basicCost[row,DetectColumn] ^= 99 & DetectColumn ^= row then do;
		column = DetectColumn;
		if MOD(num,2)=1 then do;
			stepNum[1,num] = StepCost[row,column];
		end;
		else do;
		stepNum[1,num] = StepCost[row,column];
		end;
		num = num + 1;
		DetectColumn = 99;
		end;
		else do;
		if row = 2 then do;		
		minOfStep = min(stepNum);
		StepCost[2,1] = minOfStep;
		StepCost[1,1] = StepCost[1,1] - minOfStep;
		StepCost[1,2] = StepCost[1,2] + minOfStep;
		StepCost[2,2] = StepCost[2,2] - minOfStep;
		
		DetectColumn = 99;
		end;
		DetectColumn = DetectColumn + 1;
		end;
	end;

finish;


mainRow = 2;
mainColumn = 1;

row = 2;
column = 1;
StepCost = DemandCost;
stepNum = {0 0 0};
num = 1;


run detectRows(row,column,basicCost,StepCost,stepNum,num,nrows,ncols);
run detectColumns(row,column,basicCost,StepCost,stepNum,num,nrows,ncols);
run detectRows(row,column,basicCost,StepCost,stepNum,num,nrows,ncols);
run detectColumns(row,column,basicCost,StepCost,stepNum,num,nrows,ncols);



print StepCost,stepNum;


quit;




