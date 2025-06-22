package com.sgm.navi.service.adapter.layer.bls.utils;

import com.android.utils.ConvertUtils;
import com.autonavi.gbl.layer.model.BizSearchAlongWayPoint;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

/**
 * 地图数据采样器，根据当前地图比例尺动态调整显示的数据量
 */
public class MapDataSampler<T> {

    private static final int MIN_SCALE = 3;     // 最小比例尺（显示范围最大）
    private static final int MAX_SCALE = 20;    // 最大比例尺（显示范围最小）
    private static final int MAX_DISPLAY_COUNT = 10;  // 最小比例尺显示数据量（限制为10）
    private static final int MIN_DISPLAY_COUNT = 20;    // 最大比例尺显示数据量 此处数据量过大时无法全部展示扎标 待优化算法

    private ArrayList<BizSearchAlongWayPoint> originalData = new ArrayList<>();  // 原始完整数据
    private double currentScale = 15.0;  // 默认比例尺

    // 采样策略接口
    public interface SamplingStrategy {
        <T> ArrayList<BizSearchAlongWayPoint> sample(ArrayList<BizSearchAlongWayPoint> data, int targetCount);
    }

    // 随机采样策略（默认）
    private SamplingStrategy samplingStrategy = new RandomSamplingStrategy();

    // 随机采样策略实现
    public static class RandomSamplingStrategy implements SamplingStrategy {
        private final Random random = new Random();

        @Override
        public <T> ArrayList<BizSearchAlongWayPoint> sample(ArrayList<BizSearchAlongWayPoint> data, int targetCount) {
            if (data == null || data.isEmpty() || targetCount <= 0) {
                return new ArrayList<>();
            }

            if (targetCount >= data.size()) {
                return new ArrayList<>(data);
            }

            ArrayList<BizSearchAlongWayPoint> result = new ArrayList<>(targetCount);
            List<Integer> indices = new ArrayList<>(data.size());
            for (int i = 0; i < data.size(); i++) {
                indices.add(i);
            }

            // 随机打乱索引
            Collections.shuffle(indices, random);

            // 取前targetCount个索引对应的数据
            for (int i = 0; i < targetCount; i++) {
                result.add(data.get(indices.get(i)));
            }

            return result;
        }
    }

    // 均匀间隔采样策略
    public static class UniformSamplingStrategy implements SamplingStrategy {
        @Override
        public <T> ArrayList<BizSearchAlongWayPoint> sample(ArrayList<BizSearchAlongWayPoint> data, int targetCount) {
            if (data == null || data.isEmpty() || targetCount <= 0) {
                return new ArrayList<>();
            }

            if (targetCount >= data.size()) {
                return new ArrayList<>(data);
            }

            ArrayList<BizSearchAlongWayPoint> result = new ArrayList<>(targetCount);
            double step = (double) data.size() / targetCount;

            for (int i = 0; i < targetCount; i++) {
                int index = (int) (i * step);
                result.add(data.get(Math.min(index, data.size() - 1)));
            }

            return result;
        }
    }

    // 设置采样策略
    public void setSamplingStrategy(SamplingStrategy strategy) {
        this.samplingStrategy = strategy != null ? strategy : new RandomSamplingStrategy();
    }

    // 设置原始数据
    public void setOriginalData(ArrayList<BizSearchAlongWayPoint> data) {
        this.originalData = data != null ? new ArrayList<>(data) : new ArrayList<>();
    }

    // 清空原始数据
    public void clearOriginalData() {
        this.originalData.clear();
    }

    // 更新当前比例尺
    public void updateScale(double scale) {
        // 限制比例尺在有效范围内
        this.currentScale = Math.max(MIN_SCALE, Math.min(MAX_SCALE, scale));
    }

    // 根据当前比例尺计算应该显示的数据量
    private int calculateDisplayCount() {
        // 将比例尺映射到显示数量（比例尺越小，显示数量越多）
        // 使用非线性映射，使小比例尺时显示数量增长更平缓
        double normalizedScale = (currentScale - MIN_SCALE) / (MAX_SCALE - MIN_SCALE);
        double displayCount = MAX_DISPLAY_COUNT - (MAX_DISPLAY_COUNT - MIN_DISPLAY_COUNT) * normalizedScale * normalizedScale;

        // 确保显示数量在合理范围内
        return (int) Math.max(MIN_DISPLAY_COUNT, Math.min(MAX_DISPLAY_COUNT, displayCount));
    }

    // 获取当前应显示的数据
    public ArrayList<BizSearchAlongWayPoint> getDisplayData() {
        int targetCount = calculateDisplayCount();
        ArrayList<BizSearchAlongWayPoint> sample = (ArrayList<BizSearchAlongWayPoint>) samplingStrategy.sample(originalData, targetCount);
        if (ConvertUtils.isEmpty(sample)) {
            sample = new ArrayList<>();
        }
        return sample;
    }

    // 获取当前设置的比例尺
    public double getCurrentScale() {
        return currentScale;
    }

    // 获取计算出的目标显示数量
    public int getTargetDisplayCount() {
        return calculateDisplayCount();
    }
}