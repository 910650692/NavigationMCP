package com.sgm.navi.hmi.map;

import android.annotation.SuppressLint;
import android.app.Application;
import android.graphics.drawable.Drawable;
import android.view.Gravity;
import android.view.KeyEvent;
import android.view.MotionEvent;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.car.ui.utils.DirectManipulationHelper;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BuildConfig;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.navi.AuthorizationRequestDialog;
import com.sgm.navi.service.define.map.MainScreenMapView;
import com.sgm.navi.service.define.mfc.MfcController;

import java.util.concurrent.ScheduledFuture;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public class MapViewModel extends BaseMapViewModel {

    private static final String TAG = "Cadillac MapViewModel";
    private String jsonPath = BuildConfig.MAP_SDK + "/cadillac_maparea.json";
    /** Whether this view is in DM mode. */
    private boolean mInDirectManipulationMode;
    private ScheduledFuture mMapViewScheduledFuture;
    private ScheduledFuture mImgViewScheduledFuture;
    @SuppressLint("StaticFieldLeak")
    private MainScreenMapView mapView;

    private ObservableField<Drawable> mMFCImgDrawable;

    public ObservableField<Drawable> getMFCImgDrawable() {
        return mMFCImgDrawable;
    }

    private ObservableField<Boolean> mMFCImgVisibility;

    public ObservableField<Boolean> getMFCImgVisibility() {
        return mMFCImgVisibility;
    }

    private ObservableField<Boolean> mMFCDModeVisibility;

    public ObservableField<Boolean> getMFCDModeVisibility() {
        return mMFCDModeVisibility;
    }

    public MapViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mMFCImgDrawable = new ObservableField<>(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_map_mfc_default));
        mMFCImgVisibility = new ObservableField<>(false);
        mMFCDModeVisibility = new ObservableField<>(false);
        initMFC();
    }

    public void initVisibleAreaPoint(){
        mModel.loadVisibleAreaJson(jsonPath);
    }

    public boolean showNdGoHomeView(){
        return false;
    }

    private void initMFC() {
        if (!ConvertUtils.isEmpty(mView) && !ConvertUtils.isEmpty(mView.getMainMapView())) {
            mapView = mView.getMainMapView();
            mapView.setOnFocusChangeListener((view, b) -> {
                if (Boolean.FALSE.equals(b)) {
                    //地图失去焦点，取消倒计时
                    cancelMapViewTimer();
                    mMFCImgVisibility.set(false);
                } else {
                    Logger.i(TAG, "MFC: Map 获取焦点");
                    setDModeMap(false);
                }
            });
            mapView.setOnKeyListener((view, keyCode, keyEvent) -> {
                boolean isActionUp = keyEvent.getAction() == KeyEvent.ACTION_UP;
                switch (keyCode) {
                    case KeyEvent.KEYCODE_DPAD_CENTER:
                        if (isActionUp) {
                            if (!mInDirectManipulationMode) {
                                Logger.i(TAG, "MFC Map Click Center in");
                                setDModeMap(true);
                                initMapViewTimer();
                            } else {
                                Logger.i(TAG, "MFC Map Click Center out");
                                setDModeMap(false);
                            }
                            return true;
                        }
                        return false;
                    case KeyEvent.KEYCODE_BACK:
                        if (!mInDirectManipulationMode) {
                            return false;
                        }
                        Logger.i(TAG, "MFC Map Back");
                        setDModeMap(false);
                        return true;
                    case KeyEvent.KEYCODE_DPAD_UP:
                        if (!mInDirectManipulationMode) {
                            return false;
                        }
                        Logger.i(TAG, "MFC Map Move Up");
                        initMapViewTimer();
                        mMFCImgVisibility.set(true);
                        mMFCDModeVisibility.set(true);
                        mMFCImgDrawable.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_map_mfc_up));
                        initImgViewTimer();
                        mModel.mfcMoveMap(MfcController.UP, 50);
                        return true;
                    case KeyEvent.KEYCODE_DPAD_DOWN:
                        if (!mInDirectManipulationMode) {
                            return false;
                        }
                        Logger.i(TAG, "MFC Map Move Down");
                        initMapViewTimer();
                        mMFCImgVisibility.set(true);
                        mMFCDModeVisibility.set(true);
                        mMFCImgDrawable.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_map_mfc_down));
                        initImgViewTimer();
                        mModel.mfcMoveMap(MfcController.DOWN, 50);
                        return true;
                    case KeyEvent.KEYCODE_DPAD_LEFT:
                        if (!mInDirectManipulationMode) {
                            return false;
                        }
                        Logger.i(TAG, "MFC Map Move Left");
                        initMapViewTimer();
                        mMFCImgVisibility.set(true);
                        mMFCDModeVisibility.set(true);
                        mMFCImgDrawable.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_map_mfc_left));
                        initImgViewTimer();
                        mModel.mfcMoveMap(MfcController.LEFT, 50);
                        return true;
                    case KeyEvent.KEYCODE_DPAD_RIGHT:
                        if (!mInDirectManipulationMode) {
                            return false;
                        }
                        Logger.i(TAG, "MFC Map Move Right");
                        initMapViewTimer();
                        mMFCImgVisibility.set(true);
                        mMFCDModeVisibility.set(true);
                        mMFCImgDrawable.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_map_mfc_right));
                        initImgViewTimer();
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
                mMFCImgVisibility.set(true);
                mMFCDModeVisibility.set(true);
                mMFCImgDrawable.set(ResourceUtils.Companion.getInstance().getDrawable(
                        scroll > 0 ? R.drawable.img_map_mfc_zoom_in : R.drawable.img_map_mfc_zoom_out));
                initImgViewTimer();
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
        mMapViewScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            setDModeMap(false);
        }, 5, 5);
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

    /***
     * 页面倒计时
     */
    private void initImgViewTimer() {
        cancelImgViewTimer();
        mImgViewScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            mMFCDModeVisibility.set(false);
            mMFCImgVisibility.set(false);
        }, 3, 3);
    }
    /***
     * 取消页面倒计时
     */
    private void cancelImgViewTimer() {
        if (!ConvertUtils.isEmpty(mImgViewScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mImgViewScheduledFuture);
            mImgViewScheduledFuture = null;
        }
    }

    private void setDModeMap(final boolean isDMode) {
        Logger.i(TAG, isDMode ? "进入DM模式，5s内可以操作地图" : "退出DM模式，用户不可以操作地图");
        mInDirectManipulationMode = isDMode;
        if (!ConvertUtils.isEmpty(mapView)) {
            DirectManipulationHelper.enableDirectManipulationMode(mapView, isDMode);
        }
        if (Boolean.FALSE.equals(isDMode)) {
            cancelMapViewTimer();
            mMFCImgVisibility.set(true);
            mMFCDModeVisibility.set(false);
            initImgViewTimer();
        }
    }

    /**
     * 显示隐私授权弹框
     * @param dialog
     */
    public void showAuthorizationRequestDialog(AuthorizationRequestDialog dialog) {
        if (dialog != null) {
            dialog.showDialog(Gravity.START | Gravity.BOTTOM);
        }
    }
}
