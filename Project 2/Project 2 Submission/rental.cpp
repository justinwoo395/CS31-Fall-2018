#include <iostream>
#include <string>
using namespace std;

int main()
{
    double charge, seasonalRate;
	int odoStart, odoEnd, days, mileage, month;
	string customer, hyphens = "---\n";
	char isLuxury;
	
	cout.setf(ios::fixed);
	cout.precision(2);
	
	cout << "Odometer at start: ";
    cin >> odoStart;
	if (odoStart < 0)
	{
		cout << hyphens << "The starting odometer reading must be nonnegative." << endl;
		return 1;
	}
	cout << "Odometer at end: ";
    cin >> odoEnd;
	if (odoEnd < odoStart)
	{
		cout << hyphens << "The final odometer reading must be at least as large as the starting reading." << endl;
		return 1;
	}
	cout << "Rental days: ";
	cin >> days;
	if (days <= 0)
	{
		cout << hyphens<< "The number of rental days must be positive." << endl;
		return 1;
	}
	cout << "Customer name: ";
	cin.ignore(10000, '\n');
	getline(cin, customer);
	if (customer == "")
	{
		cout << hyphens << "You must enter a customer name." << endl;
		return 1;
	}
	cout << "Luxury car? (y/n): ";
	cin >> isLuxury;
	if (not(isLuxury == 'y' or isLuxury == 'n'))
	{
		cout << hyphens << "You must enter y or n." << endl;
		return 1;
	}
	cout << "Month (1=Jan, 2=Feb, etc.): ";
	cin >> month;
	if (not(month >= 1 and month <= 12))
	{
		cout << hyphens << "The month number must be in the range 1 through 12." << endl;
		return 1;
	}
	
    mileage = odoEnd - odoStart;
    if (isLuxury == 'y')
		charge = 61 * days;
	else
        charge = 33 * days;
	
    if (mileage < 100)
    {
        charge += (0.27 * mileage);
        mileage = 0;
    }
    else
    {
        charge += (0.27 * 100);
        mileage -= 100;
    }
	
	if (month >= 4 && month <= 11)
		seasonalRate = 0.21;
	else
		seasonalRate = 0.27;
	if (mileage < 300)
    {
		charge += seasonalRate * mileage;
        mileage = 0;
    }
    else
    {
		charge += (seasonalRate * 300);
        mileage -= 300;
    }
	
    if (mileage > 0)
        charge += (0.19 * mileage);
	
	cout << hyphens << "The rental charge for " << customer << " is $" << charge << endl;
	
    return 0;
	
}



