package com.fy.navi.service.adapter.layer.bls.texture;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.layer.model.BizSearchAlongWayPoint;
import com.fy.navi.service.MapDefaultFinalTag;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

/**
 * 沿途搜扎标数据分发管理
 */
public final class MarkerDispatchManager {
    public interface MarkerDispatchCallback {

        void onMarkerDispatchCallback(List<IndexedData<BizSearchAlongWayPoint>> alongWayPoints);

        default void onMarkerDispatchCallback(BizSearchAlongWayPoint wayPoints) {

        }
    }

    private final String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private MarkerDispatchManager() {
    }

    public static MarkerDispatchManager get() {
        return MarkerDispatchManager.Holder._INSTANCE;
    }

    private static final class Holder {
        public static final MarkerDispatchManager _INSTANCE = new MarkerDispatchManager();
    }


    private final ArrayList<BizSearchAlongWayPoint> mAlongWayPoints = new ArrayList<>();
    private final CopyOnWriteArrayList<MarkerDispatchCallback> mCallbacks = new CopyOnWriteArrayList<>();

    private static final int MAX_NUM = 20; //最大扎标数量

    private ScheduledFuture mScheduledFuture;

    public void initDispatch(ArrayList<BizSearchAlongWayPoint> wayPoints, MarkerDispatchCallback callback) {
        mAlongWayPoints.clear();
        mAlongWayPoints.addAll(wayPoints);
        mCallbacks.clear();
        mCallbacks.add(callback);
        Logger.e(TAG, "updateSearchAlongRoutePoi initDispatch ");
        mScheduledFuture = ThreadManager.getInstance().asyncWithFixDelay(this::onDispatchAlongWayPoints, 0, 1, TimeUnit.SECONDS);
    }

    private void onDispatchAlongWayPoints() {
        Logger.e(TAG, "updateSearchAlongRoutePoi onDispatchAlongWayPoints ");
        if (mAlongWayPoints.isEmpty()) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            return;
        }

        Logger.e(TAG, "updateSearchAlongRoutePoi callbacks = " + mCallbacks.size());
        mCallbacks.forEach(new Consumer<MarkerDispatchCallback>() {
            @Override
            public void accept(MarkerDispatchCallback callback) {
                callback.onMarkerDispatchCallback(getMapPointsWithIndices(mAlongWayPoints));
                mAlongWayPoints.clear();
            }
        });
    }

    /**
     * 对地图沿途搜索数据进行抽稀处理并记录原始下标，确保最终只显示20个标记点
     * 规则：前4个和后3个点必须保留，中间点均匀间隔采样
     * @param originalPoints 原始数据列表
     * @return 抽稀后的数据列表
     */
    public static <T> List<IndexedData<T>> getMapPointsWithIndices(List<T> originalPoints) {
        if (originalPoints.isEmpty() || originalPoints.size() <= MAX_NUM) {
            // 如果数据量小于等于20，直接返回原始数据及其下标
            List<IndexedData<T>> result = new ArrayList<>(originalPoints.size());
            for (int i = 0; i < originalPoints.size(); i++) {
                result.add(new IndexedData<>(i, originalPoints.get(i)));
            }
            return result;
        }

        List<IndexedData<T>> resultPoints = new ArrayList<>(MAX_NUM);
        int totalSize = originalPoints.size();

        // 1. 添加前4个点
        for (int i = 0; i < 4 && i < totalSize; i++) {
            resultPoints.add(new IndexedData<>(i, originalPoints.get(i)));
        }

        // 2. 添加后3个点
        for (int i = totalSize - 3; i < totalSize; i++) {
            if (i >= 4) { // 避免与前4个点重复
                resultPoints.add(new IndexedData<>(i, originalPoints.get(i)));
            }
        }

        // 3. 计算剩余需要抽取的点数和可分配的区间
        int remainingPoints = MAX_NUM - resultPoints.size();
        int availableRangeStart = 4;
        int availableRangeEnd = totalSize - 4;
        int availableRangeSize = availableRangeEnd - availableRangeStart;

        if (remainingPoints > 0 && availableRangeSize > 0) {
            // 计算平均间隔
            double interval = (double) availableRangeSize / (remainingPoints + 1);

            // 均匀间隔采样
            for (int i = 1; i <= remainingPoints; i++) {
                int index = availableRangeStart + (int) Math.round(i * interval);
                if (index < availableRangeEnd) {
                    resultPoints.add(new IndexedData<>(index, originalPoints.get(index)));
                }
            }
        }
        Logger.d(MapDefaultFinalTag.LAYER_SERVICE_TAG, "getMapPointsWithIndices " + resultPoints.size());
        return resultPoints;
    }

    /**
     * 存储原始下标和数据的包装类
     */
    public static class IndexedData<T> {
        private final int originalIndex;
        private final T data;

        public IndexedData(int originalIndex, T data) {
            this.originalIndex = originalIndex;
            this.data = data;
        }

        public int getOriginalIndex() {
            return originalIndex;
        }

        public T getData() {
            return data;
        }
    }
}
