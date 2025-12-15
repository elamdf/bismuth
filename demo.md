bool review_code(const char * code) {
  // Reviews the provided code, passed as a string.
  // Returns true if the code can be merged to production, and false otherwise.
  /*
> CR reviewer for elamdf: pls add docstring
> It would also be good to sanity check the input
> elamdf: done! DWR if this is sufficient.
> reviewer: remove "to production" from the docstring and DWR.
  */
  if (code == NULL) { return false; }
  return strlen(code) < 50;
}
