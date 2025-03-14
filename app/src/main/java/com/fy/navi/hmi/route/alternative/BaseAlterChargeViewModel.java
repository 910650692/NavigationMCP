package com.fy.navi.hmi.route.alternative;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;
import com.fy.navi.ui.base.StackManager;

import java.text.MessageFormat;
import java.util.List;

public class BaseAlterChargeViewModel extends BaseViewModel<AlterChargeFragment, AlterChargeModel> {
    private static final String TAG = "BaseAlterChargeViewModel";
    public ObservableField<Boolean> showAlterCharge;
    /**POI详情页面**/
    public ObservableField<Boolean> routeSearchStatusVisibility;
    public ObservableField<String> routeSearchStatus;
    public ObservableField<String> routeSearchName;
    public ObservableField<String> routeSearchAddress;
    public ObservableField<String> routeSearchTimeAndDistance;
    public ObservableField<String> routeSearchElec;
    public ObservableField<Integer> routeSearchTypeVisibility;
    public ObservableField<String> routeSearchDeailsAddRemoveVia;

    public BaseAlterChargeViewModel(@NonNull Application application) {
        super(application);
        showAlterCharge = new ObservableField<>(true);
        routeSearchStatus = new ObservableField<>("");
        routeSearchName = new ObservableField<>("");
        routeSearchAddress = new ObservableField<>("");
        routeSearchTimeAndDistance = new ObservableField<>("");
        routeSearchElec = new ObservableField<>("");
        routeSearchTypeVisibility = new ObservableField<>(0);
        routeSearchDeailsAddRemoveVia = new ObservableField<>("");
    }

    @Override
    protected AlterChargeModel initModel() {
        return new AlterChargeModel();
    }

    public void requestAlterChargeStation(String poiID) {
        mModel.requestAlterChargeStation(poiID);
    }

    public void showAlterChargeStationInfo(RouteAlterChargeStationParam routeAlterChargeStationParam) {
        mView.showAlterChargeStationInfo(routeAlterChargeStationParam);
    }

    public void getSearchDetailsMode(String poiID) {
        mModel.getSearchDetailsMode(poiID);
    }

    public void addViaList(RouteAlterChargeStationInfo info) {
        mModel.addViaList(info);
    }

    public void addViaList(PoiInfoEntity poiInfoEntities) {
        mModel.addViaList(poiInfoEntities);
    }

    public void showChargeStationDetail(PoiInfoEntity poiInfoEntities) {
        mModel.getTravelTimeFuture(new GeoPoint(poiInfoEntities.getPoint().lon,poiInfoEntities.getPoint().lat))
                .thenAccept(pair -> {
                    routeSearchTimeAndDistance.set(MessageFormat.format("{0}  {1}", pair.first, pair.second));
                })
                .exceptionally(error -> {
                    Logger.d(TAG, "getTravelTimeFuture error:" + error);
                    return null;
                });
        routeSearchElec.set("20%");
        mView.showChargeStationDetail(poiInfoEntities);
    }

    // 防止点击穿透
    public Action rootClick = () -> {
    };

    public Action closePage = () -> {
        StackManager.getInstance().getCurrentFragment(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()).closeFragment(true);
    };

    public Action closeDetail = () -> {
        showAlterCharge.set(true);
    };
}
