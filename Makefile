TAGS:
	find . -type d -name ".venv" -prune \
		-o -type d -name ".ipynb_checkpoints" -prune \
		-o -type d -name ".node_modules" -prune \
		-o -type d -name "elpa" -prune \
		-o -type f -name "*.py" -print \
		-o -type f -name "*.sql" -print \
		-o -type f -name "*.el" -print \
		| etags -
