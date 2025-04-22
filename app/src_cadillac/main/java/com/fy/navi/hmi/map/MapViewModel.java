package com.fy.navi.hmi.map;

import android.annotation.SuppressLint;
import android.app.Application;
import android.view.KeyEvent;
import android.view.MotionEvent;

import androidx.annotation.NonNull;

import com.android.car.ui.utils.DirectManipulationHelper;
import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.map.MainScreenMapView;
import com.fy.navi.service.define.mfc.MfcController;

import java.util.concurrent.ScheduledFuture;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public class MapViewModel extends BaseMapViewModel {

    private static final String TAG = "Cadillac MapViewModel";
    /** Whether this view is in DM mode. */
    private boolean mInDirectManipulationMode;
    private ScheduledFuture mMapViewScheduledFuture;
    @SuppressLint("StaticFieldLeak")
    private MainScreenMapView mapView;

    public MapViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        initMFC();
    }

    private void initMFC() {
        if (!ConvertUtils.isEmpty(mView) && !ConvertUtils.isEmpty(mView.getMapView())) {
            mapView = (MainScreenMapView) mView.getMapView();
            mapView.setOnFocusChangeListener((view, b) -> {
                if (Boolean.FALSE.equals(b)) {
                    //地图失去焦点，取消倒计时
                    cancelTimer();
                } else {
                    Logger.i(TAG, "MFC: Map 获取焦点");
                    //todo 提示用户，点击确定按钮可以操作地图
                    ToastUtils.Companion.getInstance().showCustomToastView("点击中心旋钮可操作地图");
                }
            });
            mapView.setOnKeyListener((view, keyCode, keyEvent) -> {
                boolean isActionUp = keyEvent.getAction() == KeyEvent.ACTION_UP;
                switch (keyCode) {
                    case KeyEvent.KEYCODE_DPAD_CENTER:
                        if (!mInDirectManipulationMode && isActionUp) {
                            Logger.i(TAG, "MFC Map Click Center");
                            setDModeMap(true);
                            initMapViewTimer();
                        } else {
                            setDModeMap(false);
                        }
                        return true;
                    case KeyEvent.KEYCODE_BACK:
                        if (!mInDirectManipulationMode) {
                            return false;
                        }
                        Logger.i(TAG, "MFC Map Move Up");
                        setDModeMap(false);
                        return true;
                    case KeyEvent.KEYCODE_DPAD_UP:
                        if (!mInDirectManipulationMode) {
                            return false;
                        }
                        Logger.i(TAG, "MFC Map Move Up");
                        initMapViewTimer();
                        //todo 显示用户上移UI
                        mModel.mfcMoveMap(MfcController.UP, 50);
                        return true;
                    case KeyEvent.KEYCODE_DPAD_DOWN:
                        if (!mInDirectManipulationMode) {
                            return false;
                        }
                        Logger.i(TAG, "MFC Map Move Down");
                        initMapViewTimer();
                        //todo 显示用户下移UI
                        mModel.mfcMoveMap(MfcController.DOWN, 50);
                        return true;
                    case KeyEvent.KEYCODE_DPAD_LEFT:
                        if (!mInDirectManipulationMode) {
                            return false;
                        }
                        Logger.i(TAG, "MFC Map Move Left");
                        initMapViewTimer();
                        //todo 显示用户左移UI
                        mModel.mfcMoveMap(MfcController.LEFT, 50);
                        return true;
                    case KeyEvent.KEYCODE_DPAD_RIGHT:
                        if (!mInDirectManipulationMode) {
                            return false;
                        }
                        Logger.i(TAG, "MFC Map Move Right");
                        initMapViewTimer();
                        //todo 显示用户右移UI
                        mModel.mfcMoveMap(MfcController.RIGHT, 50);
                        return true;
                    default:
                        return false;
                }
            });
            // When in DM mode, rotation zooms the map.
            mapView.setOnGenericMotionListener(((view, motionEvent) -> {
                if (!mInDirectManipulationMode) {
                    return false;
                }
                float scroll = motionEvent.getAxisValue(MotionEvent.AXIS_SCROLL);
                Logger.i(TAG, scroll > 0 ? "MFC Map 顺时针旋转" : "MFC Map 逆时针旋转");
                initMapViewTimer();
                mModel.mfcChangeZoom(scroll > 0);
                return true;
            }));
        }
    }

    /***
     * 页面倒计时
     */
    private void initMapViewTimer() {
        cancelMapViewTimer();
        mMapViewScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> setDModeMap(false), 10, 10);
    }
    /***
     * 取消页面倒计时
     */
    private void cancelMapViewTimer() {
        if (!ConvertUtils.isEmpty(mMapViewScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mMapViewScheduledFuture);
            mMapViewScheduledFuture = null;
        }
    }

    private void setDModeMap(final boolean isDMode) {
        Logger.i(TAG, isDMode ? "进入DM模式，10s内可以操作地图" : "退出DM模式，用户不可以操作地图");
        String dModeText = isDMode ? "进入DM模式，10s内可以操作地图" : "已退出DM模式，用户不可以操作地图";
        ThreadManager.getInstance().postUi(() -> ToastUtils.Companion.getInstance().showCustomToastView(dModeText));
        mInDirectManipulationMode = isDMode;
        if (!ConvertUtils.isEmpty(mapView)) {
            DirectManipulationHelper.enableDirectManipulationMode(mapView, isDMode);
        }
        if (Boolean.FALSE.equals(isDMode)) {
            cancelMapViewTimer();
        }
    }
}
