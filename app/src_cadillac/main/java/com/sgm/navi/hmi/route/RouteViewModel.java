package com.sgm.navi.hmi.route;

import android.app.Application;
import android.view.KeyEvent;
import android.view.View;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;

public class RouteViewModel extends BaseRouteViewModel {
    private static final String TAG = "Cadillac RouteViewModel";

    public RouteViewModel(final Application application) {
        super(application);
        setMsgDialogTop(145);
        setMsgDialogLeft(1150);
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
        root.setOnFocusChangeListener((view, b) -> {
            if (isRoutePage()) {
                cancelTimer();
            }
        });
    }

    private boolean isRoutePage() {
        return NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.SELECT_ROUTE);
    }
}
