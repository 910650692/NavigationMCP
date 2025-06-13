package com.fy.navi.service.adapter.layer.bls.texture;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.layer.model.BizSearchAlongWayPoint;
import com.fy.navi.service.MapDefaultFinalTag;

import java.util.ArrayList;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

/**
 *
 */
public final class MarkerDispatchManager {
    public interface MarkerDispatchCallback {

        void onMarkerDispatchCallback(ArrayList<BizSearchAlongWayPoint> wayPoints);

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


    private final ArrayList<BizSearchAlongWayPoint> alongWayPoints = new ArrayList<>();
    private final CopyOnWriteArrayList<MarkerDispatchCallback> callbacks = new CopyOnWriteArrayList<>();

    private int batchSize = 10; // 每批加载的扎点数量，默认值

    private ScheduledFuture scheduledFuture;

    public void initDispatch(ArrayList<BizSearchAlongWayPoint> wayPoints, MarkerDispatchCallback callback) {
        alongWayPoints.clear();
        alongWayPoints.addAll(wayPoints);
        callbacks.add(callback);
        Logger.e(TAG, "updateSearchAlongRoutePoi initDispatch ");
        scheduledFuture = ThreadManager.getInstance().asyncWithFixDelay(this::onDispatchAlongWayPoints, 0, 1, TimeUnit.SECONDS);
    }

    private void onDispatchAlongWayPoints() {
        Logger.e(TAG, "updateSearchAlongRoutePoi onDispatchAlongWayPoints ");
        if (alongWayPoints.size() <= 0) {
            ThreadManager.getInstance().cancelDelayRun(scheduledFuture);
            return;
        }

        Logger.e(TAG, "updateSearchAlongRoutePoi callbacks = " + callbacks.size());
        callbacks.forEach(new Consumer<MarkerDispatchCallback>() {
            @Override
            public void accept(MarkerDispatchCallback callback) {
                callback.onMarkerDispatchCallback(alongWayPoints);
                alongWayPoints.clear();
            }
        });
    }
}
