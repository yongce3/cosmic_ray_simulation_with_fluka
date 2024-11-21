# FLUKA环境变量
FLUPRO = $(FLUPRO)
# 三个任务
all: step1_primary step1_surface step2_run
#任务一：生成初级宇宙射线信息
step1_primary:
	cd step1_primary && \
	$(FLUPRO)/bin/fff fluscw.f && \
	$(FLUPRO)/bin/lfluka -o myfluka -m fluka fluscw.o && \
	$(FLUPRO)/bin/rfluka -N0 -M1 *.inp -e myfluka
#任务二：生成地表宇宙射线信息
step1_surface:
	cd step1_surface && \
	$(FLUPRO)/bin/fff fluscw.f && \
	$(FLUPRO)/bin/lfluka -o myfluka -m fluka fluscw.o && \
	$(FLUPRO)/bin/rfluka -N0 -M1 *.inp -e myfluka && \
	mv try001_second_source2_info.dat ../step2/source.dat
# 任务三：生成闪烁体能量沉积信息
step2_run:
	cd step2 && \
	$(FLUPRO)/bin/fff source.f && \
	$(FLUPRO)/bin/lfluka -o myfluka -m fluka source.o && \
	$(FLUPRO)/bin/rfluka -N0 -M1 *.inp -e myfluka

# 额外任务：清理链接文件和生成的可执行文件
clean:
	rm -f step1_primary/*.o step1_primary/myfluka \
	      step1_surface/*.o step1_surface/myfluka \
	      step2/*.o step2/myfluka step2/source.dat



