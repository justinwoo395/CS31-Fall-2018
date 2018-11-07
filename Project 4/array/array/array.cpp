#include <iostream>
#include <string>
#include <cassert>
using namespace std;

//prototype declarations
int appendToAll(string a[], int n, string value);
int lookup(const string a[], int n, string target);
int positionOfMax(const string a[], int n);
int rotateLeft(string a[], int n, int pos);
int countRuns(const string a[], int n);
int flip(string a[], int n);
int differ(const string a1[], int n1, const string a2[], int n2);
int subsequence(const string a1[], int n1, const string a2[], int n2);
int lookupAny(const string a1[], int n1, const string a2[], int n2);
int divide(string a[], int n, string divider);
void swap(string& s1, string& s2);

int main()
{
	string people[5] = { "dianne", "fiona", "ed", "xavier", "greg" };
	assert(appendToAll(people, 5, "!!!") == 5);
	assert(appendToAll(people, -3, "!!!") == -1);
	assert(appendToAll(people, 0, "!!!") == 0);
	
	//test lookup
	string s[4] = {"Bob", "Jim", "Sam", "Ryan"};
	assert(lookup(s, 4, "Jim") == 1);
	assert(lookup(s, 4, "sAM") == -1);
	assert(lookup(s, 4, "Cole") == -1);
	assert(lookup(s, 4, "Bob") == 0);
	assert(lookup(s, -2, "Sam") == -1);
	assert(lookup(s, 0, "Jim") == -1);
	string tz[4] = {"car", "boat", "car", "flower"};
	assert(lookup(tz, 4, "car") == 0);
	
	//test positionOfMax
	string candidate[6] = { "dianne", "fiona", "gavin", "xavier", "ed", "betty" };
	assert(positionOfMax(candidate, 6) == 3);
	assert(positionOfMax(candidate, -1) == -1);
	assert(positionOfMax(candidate, 0) == -1);
	string colors[4] = {"violet", "blue", "green", "brown" };
	assert(positionOfMax(colors, 4) == 0);
	string animals[3] = {"fish", "cat", "zebra"};
	assert(positionOfMax(animals, 3) == 2);
	string repeat[4] = {"abc", "abc", "abc", "abc"};
	assert(positionOfMax(repeat, 4) == 0);
	
	//test rotateLeft
	string politician[5] = { "eleni", "dianne", "fiona", "kevin", "gavin" };
	assert(rotateLeft(politician, 5, 1) == 1);
	assert(rotateLeft(politician, -3, 4) == -1);
	assert(rotateLeft(politician, 0, 2) == -1);
	assert(rotateLeft(politician, 5, -1) == -1);
	string politician2[5] = { "eleni", "dianne", "fiona", "kevin", "gavin" };
	assert(rotateLeft(politician2, 5, 4) == 4);
	string politician3[5] = { "eleni", "dianne", "fiona", "kevin", "gavin" };
	assert(rotateLeft(politician3, 5, 0) == 0);
	
	//test countRuns
	string d[9] = {"xavier", "betty", "john", "john", "ed", "ed", "ed", "john", "john"};
	assert(countRuns(d, 9) == 5);
	assert(countRuns(d, 3) == 3);
	assert(countRuns(d, 0) == 0);
	assert(countRuns(d, -2) == -1);
	string water[4] = {"water", "water", "water", "water"};
	assert(countRuns(water, 4) == 1);
	
	//test flip
	string folks1[6] = { "betty", "john", "0", "xavier", "kevin", "dianne" };
	assert(flip(folks1, 6) == 6);
	assert(flip(folks1, 4) == 4);
	assert(flip(folks1, 3) == 3);
	assert(flip(folks1, 0) == 0);
	assert(flip(folks1, -1) == -1);
	
	//test differ
	string folks[6] = { "betty", "john", "", "xavier", "kevin", "dianne" };
	string group[5] = { "betty", "john", "dianne", "", "xavier" };
	assert(differ(folks, 6, group, 5) == 2);
	assert(differ(folks, 3, group, 5) == 2);
	assert(differ(folks, 2, group, 1) == 1);
	assert(differ(folks, 2, group, 2) == 2);
	assert(differ(folks, 3, group, 3) == 2);
	string fruit[3] = {"apple", "pear", "strawberry"};
	string vegetables[4] = {"lettuce", "cauliflower", "broccoli", "spinach"};
	assert(differ(fruit, 3, vegetables, 4) == 0);
	assert(differ(fruit, -1, vegetables, 5) == -1);
	assert(differ(fruit, 2, vegetables, -5) == -1);
	string names[5] = {"Josh", "Zack", "Brian", "Neil", "Jim"};
	string moreNames[5] = {"Cameron", "Zack", "Brian", "Neil", "Sam"};
	assert(differ(names, 4, moreNames, 4) == 0);
	assert(differ(names, 0, moreNames, 4) == 0);
	assert(differ(names, 4, moreNames, 0) == 0);
	
	//test subsequence
	string names0[10] = { "eleni", "gavin", "kevin", "greg", "betty", "fiona" };
	string names1[10] = { "gavin", "kevin", "greg" };
	string names2[10] = { "eleni", "greg" };
	string names6[10] = {"betty", "fiona", "cole"};
	assert(subsequence(names0, 6, names1, 3) == 1);
	assert(subsequence(names0, 4, names1, 3) == 1);
	assert(subsequence(names0, 0, names1, 3) == -1);
	assert(subsequence(names0, 3, names1, 0) == 0);
	assert(subsequence(names0, -1, names1, 3) == -1);
	assert(subsequence(names0, 2, names1, -3) == -1);
	assert(subsequence(names0, 5, names2, 2) == -1);
	assert(subsequence(names0, 0, names1, 0) == 0);
	assert(subsequence(names0, 6, names6, 3) == -1);
	
	//test lookupAny
	string names3[10] = { "eleni", "gavin", "kevin", "greg", "betty", "fiona" };
	string set1[10] = { "dianne", "betty", "greg", "gavin" };
	string set2[10] = { "xavier", "ed" };
	assert(lookupAny(names3, 6, set1, 4) == 1);
	assert(lookupAny(names3, 6, set1, 1) == -1);
	assert(lookupAny(names3, 0, set1, 4) == -1);
	assert(lookupAny(names3, 6, set1, 0) == -1);
	assert(lookupAny(names3, -1, set1, 4) == -1);
	assert(lookupAny(names3, 6, set1, -7) == -1);
	
	//test divide
	string candidate1[6] = { "dianne", "fiona", "gavin", "xavier", "ed", "betty" };
	assert(divide(candidate1, 6, "eleni") == 3);
	string candidate2[6] = { "dianne", "fiona", "gavin", "xavier", "ed", "betty" };
	assert(divide(candidate2, 6, "zack") == 6);
	string candidate3[6] = { "dianne", "fiona", "gavin", "xavier", "ed", "betty" };
	assert(divide(candidate3, 6, "andy") == 0);
	string candidate4[6] = { "dianne", "fiona", "gavin", "xavier", "ed", "betty" };
	assert(divide(candidate4, 0, "fiona") == 0);
	string candidate5[6] = { "dianne", "fiona", "gavin", "xavier", "ed", "betty" };
	assert(divide(candidate5, -2, "ed") == -1);
	string candidate6[4] = {"gavin", "kevin", "fiona", "john"};
	assert(divide(candidate6, 4, "john") == 2);
	
	//test cases provided in spec
	string h[7] = { "greg", "gavin", "ed", "xavier", "", "eleni", "fiona" };
	assert(lookup(h, 7, "eleni") == 5);
	assert(lookup(h, 7, "ed") == 2);
	assert(lookup(h, 2, "ed") == -1);
	assert(positionOfMax(h, 7) == 3);
	
	string g[4] = { "greg", "gavin", "fiona", "kevin" };
	assert(differ(h, 4, g, 4) == 2);
	assert(appendToAll(g, 4, "?") == 4 && g[0] == "greg?" && g[3] == "kevin?");
	assert(rotateLeft(g, 4, 1) == 1 && g[1] == "fiona?" && g[3] == "gavin?");
	
	string e[4] = { "ed", "xavier", "", "eleni" };
	assert(subsequence(h, 7, e, 4) == 2);
	
	string p[5] = { "gavin", "gavin", "gavin", "xavier", "xavier" };
	assert(countRuns(p, 5) == 2);
	
	string f[3] = { "fiona", "ed", "john" };
	assert(lookupAny(h, 7, f, 3) == 2);
	assert(flip(f, 3) == 3 && f[0] == "john" && f[2] == "fiona");
	
	string h1[7] = { "greg", "gavin", "ed", "xavier", "", "eleni", "fiona" };
	assert(divide(h1, 7, "fiona") == 3);
	
	cerr << "All tests succeeded" << endl;
}

//Append value to the end of each of the n elements of the array and return n.
int appendToAll(string a[], int n, string value)
{
	if (n < 0)
		return -1;	//return -1 if n is negative
	
	for (int i = 0; i < n; i++)
		a[i] += value;	//concatenate string at current position in array with value
	
	return n;
}

/* Return the position of a string in the array that is equal to target; if there is more than one such string, return the smallest position number of such a matching string. Return −1 if there is no such string. Case sensitive. */
int lookup(const string a[], int n, string target)
{
	for (int i = 0; i < n; i++)
		if (a[i] == target)	//if target found in array, return current position in string
			return i;
	
	return -1;	//if target not found, return -1
}

/* Return the position of a string in the array such that that string is >= every string in the array. If there is more than one such string, return the smallest position number of such a string. Return −1 if the array has no interesting elements */
int positionOfMax(const string a[], int n)
{
	if (n <= 0)	//check if n is negative or 0 (empty array)
		return -1;
	int max = 0;	//initialize max to position 0
	
	for (int i = 1; i < n; i++)	//skip 1st element
		if (a[max] < a[i])
			max = i;
	
	return max;
}

/* Eliminate the item at position pos by copying all elements after it one place to the left. Put the item that was thus eliminated into the last position of the array. Return the original position of the item that was moved to the end. */
int rotateLeft(string a[], int n, int pos)
{
	if (pos == n - 1)	//if pos is end of string, leave array unchanged and return pos
		return pos;
	
	if (n <= 0 || pos < 0)	//check if array length or position are negative
		return -1;
	
	string temp; //container to temporarily store string at pos
	
	for (int i = 0; i < n; i++)
	{
		if (i == pos)
			temp = a[i]; //copy string at pos to temp string because it will be overwritten later
		if (i > pos)
		{
			a[i - 1] = a[i]; //overwrite string at previous position with value of string at current position
			if (i == n - 1) //check if last string in array reached
			{
				a[i] = temp; //store string initially at pos in last string of array
				return pos;
			}
		}
	}
	
	return -1;
}

//Return the number of sequences of one or more consecutive identical items in a.
int countRuns(const string a[], int n)
{
	if (n < 0)
		return -1;
	
	int count = 0; //initialize counter to 0
	
	for (int i = 0; i < n; i++)
		if (i == (n - 1) || a[i] != a[i + 1]) //increment count if last string in array or different from following string
			count++;
	
	return count;
}

//Reverse the order of the elements of the array and return n.
int flip(string a[], int n)
{
	if (n < 0)
		return -1;
	
	for (int i = 0; i < n/2; i++) //mirror items in array around center position (half of length)
		swap(a[i], a[n - (i + 1)]); //swap elements that are the same distance in from array boundaries
	//offset by 1 to account for array index starting at 0
	return n;
}

/* Return the position of the first corresponding elements of a1 and a2 that are not equal. n1 is the number of interesting elements in a1, and n2 is the number of interesting elements in a2. If the arrays are equal up to the point where one or both runs out, return whichever value of n1 and n2 is less than or equal to the other */
int differ(const string a1[], int n1, const string a2[], int n2)
{
	if (n1 < 0 || n2 < 0)	//check if either n1 or n2 is negative
		return -1;
	
	int n;
	//set n to lesser of n1 and n2 to prevent accessing values outside array
	if (n1 < n2)
		n = n1; //sets n to n1 if it is less than n2
	else
		n = n2; //if n2 is greater than or equal to n1, set n to n2
	
	for (int i = 0; i < n; i++)
		if (a1[i] != a2[i])	//if corresponding elements of a1 and a2 are not equal, return current position
			return i;
	
	return n;
}

/* If all n2 elements of a2 appear in a1, consecutively and in the same order, then return the position in a1 where that subsequence begins. If the subsequence appears more than once in a1, return the smallest such beginning position in the array. Return −1 if a1 does not contain a2 as a contiguous subsequence. */
int subsequence(const string a1[], int n1, const string a2[], int n2)
{
	if (n1 < 0 || n2 < 0)	//check if either array has invalid length
		return -1;
	if (n2 == 0)	//sequence of 0 elements is a subsequence of any sequence
		return 0;
	
	bool hasSubsequence = false; //set value to false by default
	
	for (int i = 0; i < n1; i++)
	{
		if (a1[i] == a2[0] && i + n2 <= n1) //check if subsequence a2 could begin at current position in a1
		{
			hasSubsequence = true; //set hasSubsequence to true at beginning of test
			for (int j = 0; j < n2; j++)
				if (a1[i + j] != a2[j]) //if string in a1 is not equal to corresponding string in a2, stop testing
					hasSubsequence = false;
		}
		if (hasSubsequence) //if a1 contains a2, return starting position of subsequence in a1
			return i;
	}
	
	return -1;
}

//Return the smallest position in a1 of an element that is equal to any of the n2 elements in a2. Return −1 if no element of a1 is equal to any element of a2.
int lookupAny(const string a1[], int n1, const string a2[], int n2)
{
	if (n1 <= 0 || n2 <= 0)
		return -1;
	
	for (int i = 0; i < n1; i++) //loop through all elements of a1
		for (int j = 0; j < n2; j++)
			if (a1[i] == a2[j]) //if current string in a1 matches any string in a2, return position in a1
				return i;
	
	return -1;
}

/* Rearrange the elements of the array so that all the elements whose value is < divider come before all the other elements, and all the elements whose value is > divider come after all the other elements. Return the position of the first element that, after the rearrangement, is not < divider, or n if there are no such elements. */
int divide(string a[], int n, string divider)
{
	if (n < 0)
		return -1;
	
	//modified implementation of bubble sort to rearrange array elements
	for (int i = 0; i < n; i++)
	{
		for (int j = 0; j < n - (i + 1); j++) //last i elements of array already sorted
		{
			if (a[j] > a[j + 1]) //if string greater than divider, swap with next element
				swap(a[j], a[j + 1]);
		}
	}
	
	for (int k = 0; k < n; k++)
		if (a[k] >= divider) //find position of first string greater than divider
			return k;
	
	return n;
}

//swap values of two strings
void swap(string& s1, string& s2)
{
	string temp = s1; //s1 will be overwritten, so copy it to a temporary variable
	s1 = s2;
	s2 = temp; //copy intitial value of s1 stored in temp
}
