SHELL := /bin/bash
ifeq ($(shell uname -s), Darwin)
size := stat -f "%z"
else
size := stat --printf "%s"
endif

.PHONY: test
test: ## Run poor man's snapshot tests
	@echo ${size}
	@gleam test --target=javascript -- --capture=yes 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_js_capture_yes.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff -c <(sort snapshots/new_js_capture_yes.txt) <(sort snapshots/js_capture_yes.txt)
	@rm snapshots/new_js_capture_yes.txt
	@gleam test --target=javascript -- --capture=no 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_js_capture_no.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff -c <(sort snapshots/new_js_capture_no.txt) <(sort snapshots/js_capture_no.txt)
	@rm snapshots/new_js_capture_no.txt
	@gleam test --target=javascript -- --capture=mixed 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_js_capture_mixed.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff -c <(sort snapshots/new_js_capture_mixed.txt) <(sort snapshots/js_capture_mixed.txt)
	@rm snapshots/new_js_capture_mixed.txt
	@gleam test --target=javascript -- --ignore=ignore 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_js_ignore_ignore.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff -c <(sort snapshots/new_js_ignore_ignore.txt) <(sort snapshots/js_ignore_ignore.txt)
	@rm snapshots/new_js_ignore_ignore.txt
	@gleam test --target=javascript -- --modules=subfolder/sub_test 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_js_modules_subtest.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff -c <(sort snapshots/new_js_modules_subtest.txt) <(sort snapshots/js_modules_subtest.txt)
	@rm snapshots/new_js_modules_subtest.txt
	@gleam test --target=erlang -- --capture=yes 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_erlang_capture_yes.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi; \
	if [ $$(${size} snapshots/new_erlang_capture_yes.txt) -ne $$(${size} snapshots/erlang_capture_yes.txt) ]; \
	then \
		diff -c <(sort snapshots/new_erlang_capture_yes.txt) <(sort snapshots/erlang_capture_yes.txt); \
	fi
	@rm snapshots/new_erlang_capture_yes.txt
	@gleam test --target=erlang -- --capture=no 2> snapshots/new_erlang_capture_no_stderr.txt |(sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_erlang_capture_no.txt); \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi; \
	if [ $$(${size} snapshots/new_erlang_capture_no.txt) -ne $$(${size} snapshots/erlang_capture_no.txt) ]; \
	then \
		diff -c <(sort snapshots/new_erlang_capture_no.txt) <(sort snapshots/erlang_capture_no.txt); \
	fi
	@(exit $((,$(${size} snapshots/new_erlang_capture_no_stderr.txt) == $(${size} snapshots/erlang_capture_no_stderr.txt) ? 0 : 2)))
	@rm snapshots/new_erlang_capture_no.txt
	@rm snapshots/new_erlang_capture_no_stderr.txt
	@gleam test --target=erlang -- --capture=mixed 2> snapshots/new_erlang_capture_mixed_stderr.txt |(sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_erlang_capture_mixed.txt); \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi; \
	if [ $$(${size} snapshots/new_erlang_capture_mixed.txt) -ne $$(${size} snapshots/erlang_capture_mixed.txt) ]; \
	then \
		diff -c <(sort snapshots/new_erlang_capture_mixed.txt) <(sort snapshots/erlang_capture_mixed.txt); \
	fi
	@(exit $((,$(${size} snapshots/new_erlang_capture_mixed_stderr.txt) == $(${size} snapshots/erlang_capture_mixed_stderr.txt) ? 0 : 2)))
	@rm snapshots/new_erlang_capture_mixed.txt
	@rm snapshots/new_erlang_capture_mixed_stderr.txt
	@gleam test --target=erlang -- --ignore=ignore 2> snapshots/new_erlang_ignore_ignore_stderr.txt |(sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_erlang_ignore_ignore.txt); \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi; \
	if [ $$(${size} snapshots/new_erlang_ignore_ignore.txt) -ne $$(${size} snapshots/erlang_ignore_ignore.txt) ]; \
	then \
		diff -c <(sort snapshots/new_erlang_ignore_ignore.txt) <(sort snapshots/erlang_ignore_ignore.txt); \
	fi
	@(exit $((,$(${size} snapshots/new_erlang_ignore_ignore_stderr.txt) == $(${size} snapshots/erlang_ignore_ignore_stderr.txt) ? 0 : 2)))
	@rm snapshots/new_erlang_ignore_ignore.txt
	@rm snapshots/new_erlang_ignore_ignore_stderr.txt
	@gleam test --target=erlang -- --modules=subfolder/sub_test 2> snapshots/new_erlang_modules_subtest_stderr.txt |(sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_erlang_modules_subtest.txt); \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi; \
	if [ $$(${size} snapshots/new_erlang_modules_subtest.txt) -ne $$(${size} snapshots/erlang_modules_subtest.txt) ]; \
	then \
		diff -c <(sort snapshots/new_erlang_modules_subtest.txt) <(sort snapshots/erlang_modules_subtest.txt); \
	fi
	diff -c <(sort snapshots/new_erlang_modules_subtest_stderr.txt) <(sort snapshots/erlang_modules_subtest_stderr.txt)
	@(exit $((,$(${size} snapshots/new_erlang_modules_subtest_stderr.txt) == $(${size} snapshots/erlang_modules_subtest_stderr.txt) ? 0 : 2)))
	@rm snapshots/new_erlang_modules_subtest.txt
	@rm snapshots/new_erlang_modules_subtest_stderr.txt
