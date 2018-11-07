#include <iostream>
#include <cctype>
#include <string>
#include <cassert>
using namespace std;

//prototype declarations
bool hasProperSyntax(string pollData);
int tallySeats(string pollData1, char party, int& seatTally);
bool isValidUppercaseStateCode(string stateCode);

int main()
{
	assert(!hasProperSyntax("VT3"));
	assert(hasProperSyntax("CA3r7r"));
	assert(!hasProperSyntax("V"));
	assert(!hasProperSyntax("VT999D"));
	assert(!hasProperSyntax("VT99D999r"));
	assert(!hasProperSyntax("KS4R, CA3D"));
	assert(!hasProperSyntax("ca3R5d6,vt3d6R,nj"));
	assert(!hasProperSyntax("CA3r,"));
	assert(!hasProperSyntax("CA3dr"));
	assert(!hasProperSyntax("3r6d2i"));
	assert(hasProperSyntax("CA3D00r,VT8d6r00i"));
	assert(hasProperSyntax("VT,CA"));
	assert(!hasProperSyntax("Ks4 R"));
	assert(hasProperSyntax("CT5D,NY9R17D1I,VT,ne3r00D"));
	assert(!hasProperSyntax("ZT5D,NY9R17D1I,VT,ne3r00D"));
	assert(hasProperSyntax("MA9D,KS4R")  &&  hasProperSyntax("KS4R,MA9D"));
	assert(hasProperSyntax("MA9D,,KS4R") == hasProperSyntax("KS4R,,MA9D"));
	assert(!hasProperSyntax("KS&CA"));
	assert(!hasProperSyntax("KS CA"));

	int seats;
	seats = -999;
	assert(tallySeats("", 'c', seats) == 0  &&  seats == 0);
	seats = -999;
	assert(tallySeats("CT5D,NY9R17D1I,VT,ne3r00D", 'd', seats) == 0  &&  seats == 22);
	seats = -999;
	assert(tallySeats("CT5D,NY9R17D1I,VT,ne3r00D", '%', seats) == 2  &&  seats == -999);
	seats = -999;
	assert(tallySeats("NY9R17D1I,VT,NJ3D5R4D,KS4R", 'd', seats) == 0  &&  seats == 24);
	seats = -999;
	assert(tallySeats("NY9R17D1I,VT,NJ3D5R4D,KS4R", 'r', seats) == 0  &&  seats == 18);
	seats = -999;
	assert(tallySeats("NY9r17D1I,VT,NJ3D5r4D,KS4r", 'R', seats) == 0  &&  seats == 18);
	seats = -999;
	assert(tallySeats("NY9r17D1I ,VT,NJ3D5r4D,KS4r", 'R', seats) == 1  &&  seats == -999);
	seats = -999;
	assert(tallySeats("VA7R3d,KS4R2i,VA4D", 'd', seats) == 0  &&  seats == 7);
	seats = -999;
	assert(tallySeats("VA7R,KS4R,VA4D", 'r', seats) == 0  &&  seats == 11);
	seats = -999;
	assert(tallySeats("VT,CA", 'r', seats) == 0 && seats == 0);
	seats = -999;
	assert(tallySeats("CT0D,NY9R00D1I,VT,ne3r00D", 'd', seats) == 0 && seats == 0);
	seats = -999;
	assert(tallySeats("", 'd', seats) == 0 && seats == 0);
	seats = -999;
	assert(tallySeats("VT999d", 'd', seats) == 1 && seats == -999);
	seats = -999;
//	assert(tallySeats("NY9R17D1I,VT,NJ3D5R4D,KS4R", 'd', seats) == 0 && seats == 24);

	cout << "All tests succeeded" << endl;
}

//hasProperSyntax - returns whether a poll string is properly formatted
bool hasProperSyntax(string pollData)
{
	if (pollData.length() == 0) //empty string is correct
		return true;
	
	for (auto& c : pollData)	//convert all characters in pollData to uppercase form
		c = toupper(c);
	
	int digitCounter;
	
	for (int i = 0;;i++) //loop through poll string
	{
		if (isValidUppercaseStateCode(pollData.substr(i, 2))) //check if valid state code at indicated position in string
		{
			i += 2;	//advance 2 positions (length of state code)
			while (pollData[i] != ',')	//loops until end of a state forecast
			{
				digitCounter = 0;
				while (isdigit(pollData[i]))	//detect and advance through digits of party result
				{
					if (i + 1 == pollData.length() || pollData[i + 1] == ',') //check for end of string or state forecast
						break;
					else
					{
						digitCounter++;
						i++;
					}
				}
				if (i == pollData.length())	//checks if last character has been reached
					return true;
				if (!isalpha(pollData[i]) || digitCounter > 2)	//check if character after digits is a party code
					return false;	//if invalid, exit loop
				i++;
				if (i == pollData.length())	//checks if last character has been reached
					return true;
				if (!isdigit(pollData[i]))	//if character after party code is not a digit, exit loop
					break;
			}
		}
		else break; //if invalid state code, exit loop
	}
	return false;
}

//tallySeats - sets a variable to predicted number of votes for a given party
int tallySeats(string pollData1, char party, int& seatTally)
{
	//check if poll string and party character are properly formatted
	if (!hasProperSyntax(pollData1))
		return 1;
	if (!isalpha(party))
		return 2;
	
	string sum;
	seatTally = 0;
	
	for (int j = 2; j < pollData1.length(); j += 2)	//state code known to be valid, so increments by 2 automatically
	{
		while (pollData1[j] != ',')	//advances through state forecast until comma is reached
		{
			sum = "";	//sets sum to blank string for each forecast
			while (isdigit(pollData1[j]))
			{
				sum += pollData1[j];	//append digit to sum string
				j++;
			}
			if (toupper(pollData1[j]) == toupper(party)) //check if character matches given party code
				seatTally += stoi(sum);	//converts sum string to an integer and increments seatTally by the number of votes forecasted
			j++;
			if (j == pollData1.length())	//if end of poll string reached, exit out of loop
				break;
		}
	}
	return 0;
}

bool isValidUppercaseStateCode(string stateCode)
{
    const string codes =
        "AL.AK.AZ.AR.CA.CO.CT.DE.FL.GA.HI.ID.IL.IN.IA.KS.KY."
        "LA.ME.MD.MA.MI.MN.MS.MO.MT.NE.NV.NH.NJ.NM.NY.NC.ND."
        "OH.OK.OR.PA.RI.SC.SD.TN.TX.UT.VT.VA.WA.WV.WI.WY";
    return (stateCode.size() == 2  &&
            stateCode.find('.') == string::npos  &&  // no '.' in stateCode
            codes.find(stateCode) != string::npos);  // match found
}






