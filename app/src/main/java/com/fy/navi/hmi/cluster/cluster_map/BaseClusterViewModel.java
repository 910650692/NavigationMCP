package com.fy.navi.hmi.cluster.cluster_map;

import android.app.Application;
import android.text.TextUtils;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;
import androidx.databinding.ObservableInt;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.ui.base.BaseViewModel;
import com.fy.navi.utils.ActivityCloseManager;

public class BaseClusterViewModel extends BaseViewModel<ClusterActivity, ClusterModel> {
    private static final String TAG = "BaseClusterViewModel";
    public ObservableInt routeNameVisibleStatus;
    public ObservableField<String> arriveTimeField; // 到达时间
    public ObservableField<String> arrivalDayField; // 到达天数
    public ObservableField<String> remainingMileageField; // 剩余距离
    public ObservableField<String> stvNaviRouteNameField; // 当前路名

    public BaseClusterViewModel(@NonNull Application application) {
        super(application);
        Logger.d(TAG, "BaseClusterViewModel initialized");
        ActivityCloseManager.getInstance().setOnCloseListener(() -> mView.finish());
        routeNameVisibleStatus = new ObservableInt(View.GONE);
        arriveTimeField = new ObservableField<>();
        arrivalDayField = new ObservableField<>();
        remainingMileageField = new ObservableField<>();
        stvNaviRouteNameField = new ObservableField<>();
    }

    @Override
    protected ClusterModel initModel() {
        Logger.d(TAG, "Initializing ClusterModel");
        return new ClusterModel();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy called");
        ActivityCloseManager.getInstance().removeListener();
        LayerPackage.getInstance().removeLayerService(mView.getMapView().provideMapTypeId());
        MapPackage.getInstance().unBindMapView(mView.getMapView());
        MapPackage.getInstance().destroyMapView(mView.getMapView().provideMapTypeId());
    }

    public void loadMapView(){
        MapPackage.getInstance().loadMapView(mView.getMapView());
    }

    public void updateEta(NaviEtaInfo naviEtaInfo) {
        Logger.d(TAG, "Updating ETA info");
        int distance = naviEtaInfo.getAllDist();
        int time = naviEtaInfo.getAllTime();

        if (distance <= 0 && time <= 0) {
            Logger.d(TAG, "Distance and time are both zero, skipping update");
            return;
        }
        String mArriveDay = TimeUtils.getArriveDay(time);
        String mArriveTime = TimeUtils.getArriveTime(AppCache.getInstance().getMContext(), time);
        String mRemainInfo = TimeUtils.getRemainingMileage(AppCache.getInstance().getMContext(), distance);
            Logger.i(TAG, "showArriveInfo");
            if (!TextUtils.isEmpty(mArriveTime)) {
                arriveTimeField.set(ConvertUtils.digitToBold(mArriveTime).toString());
            }
            arrivalDayField.set(mArriveDay);
            remainingMileageField.set(ConvertUtils.digitToBold(mRemainInfo).toString());
    }

    public void updateRouteName(String curRouteName) {
        Logger.d(TAG, "Updating route name");
        stvNaviRouteNameField.set(curRouteName);
    }

    public void updateNaviStatus(boolean isVisible) {
        routeNameVisibleStatus.set(isVisible ? View.VISIBLE : View.GONE);
    }
}
