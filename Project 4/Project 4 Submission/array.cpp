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

int main() {}

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
