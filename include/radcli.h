#include "parseopt/parseopt.h"

void radius_version_hook(WORDWRAP_FILE wf, struct parseopt *po);
void grad_parseopt(struct parseopt const *po_orig, int argc, char **argv,
		   int *rargc, char ***rargv);
