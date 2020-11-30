#define _POSIX_C_SOURCE 200809L

#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
#include<unistd.h>

#define MIN(a, b) ( a < b ? a : b )
#define MAX(a, b) ( a > b ? a : b )

int main (int argc, char *argv[])
{
	if ( argc != 6 )
	{
		fputs("ERROR: layout needs five arguments.\n", stderr);
		return EXIT_FAILURE;
	}

	const int   view_amount   = atoi(argv[1]);
	const int   master_amount = atoi(argv[2]);
	const float master_factor = atof(argv[3]);
	const int   width         = atoi(argv[4]);
	const int   height        = atoi(argv[5]);

	const int x = 0, y = 0;

	if ( view_amount == 0 )
		return EXIT_FAILURE;

	unsigned int master_size, stack_size, view_x, view_y, view_width, view_height;
	int left_over = view_amount - master_amount - 1;
	if ( master_amount == 0 )
	{
		master_size = 0;
		stack_size  = width;
	}
	else if ( view_amount <= master_amount )
	{
		master_size = width;
		stack_size  = 0;
	}
	else
	{
		master_size = width * master_factor;
		stack_size  = width - master_size;
	}
	for (unsigned int i = 0; i < view_amount; i++)
	{
		if ( i < master_amount ) /* Master area. */
		{
			view_x      = x;
			view_width  = master_size;
			view_height = height / MIN(master_amount, view_amount);
			view_y      = y + ( i * view_height );
		}
		else if ( i == master_amount ) /* Secondary area. */
		{
			view_x      = x + master_size;
			view_width  = stack_size;
			view_y      = 0;
			view_height = left_over == 0 ? height : 0.7 * height;
		}
		else /* Stack area. */
		{
			if ( left_over == 1 )
			{
				view_x = x + master_size;
				view_width  = stack_size;
				view_height = 0.3 * height;
				view_y      = 0.7 * height;
			}
			else
			{
				view_x = x + master_size + (0.1 * stack_size / (left_over - 1)) * (i - master_amount - 1);
				view_width  = stack_size * 0.9;
				view_height = 0.3 * height * 0.9;
				view_y      = 0.7 * height + (0.1 * (0.3 * height) / (left_over - 1)) * (i - master_amount - 1);
			}
		}

		fprintf(stdout, "%d %d %d %d\n", view_x, view_y, view_width, view_height);
	}

	return EXIT_SUCCESS;
}


