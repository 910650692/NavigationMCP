package com.sgm.navi.hmi.navi;

import android.annotation.SuppressLint;
import android.app.Application;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.sgm.navi.service.define.navi.NaviDriveReportEntity;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.ui.base.BaseViewModel;

public class NaviDriveReportViewModel extends BaseViewModel<NaviDriveReportFragment, NaviDriveReportModel> {

    public ObservableField<String> mStartPos;
    public ObservableField<String> mEndPos;
    public ObservableField<String> mDrivenTime;
    public ObservableField<String> mDrivenDist;
    public ObservableField<String> mEstimateFuelConsume;
    public ObservableField<String> mTotalFuelConsume;

    public NaviDriveReportViewModel(@NonNull Application application) {
        super(application);
        mStartPos = new ObservableField<>("");
        mEndPos = new ObservableField<>("");
        mDrivenTime = new ObservableField<>("");
        mDrivenDist = new ObservableField<>("");
        mEstimateFuelConsume = new ObservableField<>("");
        mTotalFuelConsume = new ObservableField<>("");
    }

    @Override
    protected NaviDriveReportModel initModel() {
        return new NaviDriveReportModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        NaviDriveReportEntity naviDriveReportEntity =
                NaviPackage.getInstance().getMNaviDriveReportEntity();
        setData(naviDriveReportEntity);
    }

    public void setData(NaviDriveReportEntity naviDriveReportEntity) {
        if (naviDriveReportEntity != null) {
            mStartPos.set(naviDriveReportEntity.getStartPos());
            mEndPos.set(naviDriveReportEntity.getEndPos());
            mDrivenTime.set(formatSecondsToTime(naviDriveReportEntity.getDrivenTime()));
            mDrivenDist.set(formatDistance(naviDriveReportEntity.getDrivenDist()));
            mEstimateFuelConsume.set(naviDriveReportEntity.getEstimateFuelConsume() + "L/100km");
            mTotalFuelConsume.set(naviDriveReportEntity.getTotalFuelConsume() + "L");
        }
    }

    @SuppressLint("DefaultLocale")
    private String formatSecondsToTime(int totalSeconds) {
        int hours = totalSeconds / 3600;
        int minutes = (totalSeconds % 3600) / 60;
        int seconds = totalSeconds % 60;
        return String.format("%02d:%02d:%02d", hours, minutes, seconds);
    }

    @SuppressLint("DefaultLocale")
    private String formatDistance(int meters) {
        if (meters < 1000) {
            return meters + "m";
        } else {
            double km = meters / 1000.0;
            String kmStr = String.format("%.2f", km);
            // 去除多余的0和小数点
            if (kmStr.indexOf(".") > 0) {
                kmStr = kmStr.replaceAll("0+$", ""); // 去掉末尾0
                kmStr = kmStr.replaceAll("\\.$", ""); // 去掉末尾.
            }
            return kmStr + "km";
        }
    }
}
