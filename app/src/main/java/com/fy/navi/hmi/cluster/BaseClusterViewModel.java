package com.fy.navi.hmi.cluster;

import android.app.Application;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseClusterViewModel extends  BaseViewModel<ClusterActivity, ClusterModel> {
    private final String TAG = "BaseClusterViewModel";
    public ObservableField<Boolean> naviBarVisibility;
    public ObservableField<Boolean> naviUiVisibility;//tmc
    public ObservableField<Boolean> cruiseUiVisibility;//tmc
    public ObservableField<Boolean> mPlaceHolderVisibility;//占位图

    public BaseClusterViewModel(@NonNull Application application) {
        super(application);
        naviBarVisibility = new ObservableField<>(true);
        naviUiVisibility = new ObservableField<>(true);
        cruiseUiVisibility = new ObservableField<>(true);
        mPlaceHolderVisibility = new ObservableField<>(true);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        naviBarVisibility.set(!TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING));
        naviUiVisibility.set(TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING));
        cruiseUiVisibility.set(TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.CRUISE));
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    protected ClusterModel initModel() {
        return new ClusterModel();
    }



    public void loadMapView() {
        mModel.loadMapView();
    }

    public IBaseScreenMapView getMapView() {
        return mView.getMapView();
    }

    public void onNaviStatusChanged(@NaviStatus.NaviStatusType String status) {
        naviBarVisibility.set(!TextUtils.equals(status, NaviStatus.NaviStatusType.NAVING));
        naviUiVisibility.set(TextUtils.equals(status, NaviStatus.NaviStatusType.NAVING));
        cruiseUiVisibility.set(TextUtils.equals(status, NaviStatus.NaviStatusType.CRUISE));
    }






    public void naviArriveOrStop() {
        naviUiVisibility.set(false);
        naviBarVisibility.set(true);
    }
}
