package com.fy.navi.hmi.route;

import android.app.Application;
import android.view.KeyEvent;
import android.view.View;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;

public class RouteViewModel extends BaseRouteViewModel {
    private static final String TAG = "Cadillac RouteViewModel";

    public RouteViewModel(final Application application) {
        super(application);
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onStart() {
        super.onStart();
        initMFC();
    }

    private void initMFC() {
        View root = mView.getRootViewForMFC();
        if (ConvertUtils.isEmpty(root)) {
            Logger.i(TAG, "RootView is null");
            return;
        }
        root.setOnKeyListener((view, keyCode, keyEvent) -> {
            switch (keyCode) {
                case KeyEvent.KEYCODE_DPAD_CENTER:
                case KeyEvent.KEYCODE_BACK:
                case KeyEvent.KEYCODE_DPAD_UP:
                case KeyEvent.KEYCODE_DPAD_DOWN:
                case KeyEvent.KEYCODE_DPAD_LEFT:
                case KeyEvent.KEYCODE_DPAD_RIGHT:
                    if (isRoutePage()) {
                        Logger.i(TAG, "MFC操作，取消倒计时");
                        cancelTimer();
                    }
                    return false;
                default:
                    return false;
            }
        });
        root.setOnGenericMotionListener(((view, motionEvent) -> {
            if (isRoutePage()) {
                Logger.i(TAG, "MFC操作，取消倒计时");
                cancelTimer();
            }
            return false;
        }));
    }

    private boolean isRoutePage() {
        return NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.SELECT_ROUTE);
    }
}
