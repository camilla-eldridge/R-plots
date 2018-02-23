- Plots highlighted lines by type and colours by group using ggplot. * note type is assigned in script not csv file.

- Columns for csv file must be in format: label, x, y , see test.csv
- Data to be highlighted must be in format label, pos1 pos2, see test_highlighted.txt

		
		USAGE:./highlighted_and_shaded.R  input.csv  highlight_positions.txt  plot_title  yintercept  legend_for_shaded  pos_for_shaded  output_plot.png


		Example ./highlighted_and_shaded_line.R  test_one_ag.csv  test_highlight.txt  highlighted  test_title  1  shaded 170:200,300:560 highlighted_shaded.png
 
