package com.sgm.navi.hmi.navi;
import com.android.utils.log.Logger;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.navi.NaviDriveReportEntity;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.ui.base.BaseModel;

public class NaviDriveReportModel extends BaseModel<NaviDriveReportViewModel> implements IGuidanceObserver {
    public static final String TAG = MapDefaultFinalTag.NAVI_HMI_MODEL_DRIVE_REPORT;

    @Override
    public void onCreate() {
        super.onCreate();
        NaviPackage.getInstance().registerObserver(TAG, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        NaviPackage.getInstance().unregisterObserver(TAG);
    }

    @Override
    public void onDriveReport(NaviDriveReportEntity report) {
        if (Logger.openLog) {
            Logger.i(TAG, "report = " , report.toString());
        }
        if (mViewModel != null) {
            mViewModel.setData(report);
        }
    }
}
