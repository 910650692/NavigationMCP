package com.sgm.navi.hmi.route;

import android.app.Application;

import com.sgm.navi.hmi.BuildConfig;

public class RouteViewModel extends BaseRouteViewModel {
    public RouteViewModel(final Application application) {
        super(application);
        if (BuildConfig.FLAVOR.equals("clea_local_8155")
                || BuildConfig.FLAVOR.equals("clea_8775")) {
            setMsgDialogTop((int) application.getResources()
                    .getDimension(com.sgm.navi.service.R.dimen.dp_53));
            setMsgDialogLeft((int) application.getResources()
                    .getDimension(com.sgm.navi.service.R.dimen.dp_579));
        }else{
            setMsgDialogTop(100);
            setMsgDialogLeft(700);
        }
    }

    @Override
    public boolean isNewAlterCharge() {
        return true;
    }
}
