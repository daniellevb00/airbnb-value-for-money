OUTPUT = ../../gen/analysis

all: $(OUTPUT)/model_report_airbnb.text $(OUTPUT)/model_report_airbnb.html $(OUTPUT)/model_report_airbnb_pq.text $(OUTPUT)/model_report_airbnb_pq.html

$(OUTPUT)/model_report_airbnb.text $(OUTPUT)/model_report_airbnb.html: 4_analysis_q1.R
		R --vanilla < 4_analysis_q1.R

$(OUTPUT)/model_report_airbnb_pq.text $(OUTPUT)/model_report_airbnb_pq.html: 5_analysis_q2.R
		R --vanilla < 5_analysis_q2.R