package com.fy.navi.hmi.route.alternative;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;
import com.fy.navi.ui.base.StackManager;

import java.text.MessageFormat;

public class BaseAlterChargeViewModel extends BaseViewModel<AlterChargeFragment, AlterChargeModel> {
    private static final String TAG = "BaseAlterChargeViewModel";
    private ObservableField<Boolean> mShowAlterCharge;
    /**POI详情页面**/
    private ObservableField<Boolean> mRouteSearchStatusVisibility;
    private ObservableField<String> mRouteSearchStatus;
    private ObservableField<String> mRouteSearchName;
    private ObservableField<String> mRouteSearchAddress;
    private ObservableField<String> mRouteSearchTimeAndDistance;
    private ObservableField<String> mRouteSearchElec;
    private ObservableField<Integer> mRouteSearchTypeVisibility;
    private ObservableField<String> mRouteSearchDetailAddRemoveVia;

    public ObservableField<Boolean> getShowAlterCharge() {
        return mShowAlterCharge;
    }

    public ObservableField<Boolean> getRouteSearchStatusVisibility() {
        return mRouteSearchStatusVisibility;
    }

    public ObservableField<String> getRouteSearchStatus() {
        return mRouteSearchStatus;
    }

    public ObservableField<String> getRouteSearchName() {
        return mRouteSearchName;
    }

    public ObservableField<String> getRouteSearchAddress() {
        return mRouteSearchAddress;
    }

    public ObservableField<String> getRouteSearchTimeAndDistance() {
        return mRouteSearchTimeAndDistance;
    }

    public ObservableField<String> getRouteSearchElec() {
        return mRouteSearchElec;
    }

    public ObservableField<Integer> getRouteSearchTypeVisibility() {
        return mRouteSearchTypeVisibility;
    }

    public ObservableField<String> getRouteSearchDetailAddRemoveVia() {
        return mRouteSearchDetailAddRemoveVia;
    }

    /**
     * 当前充电站花费
     **/
    private ObservableField<String> mSpend;
    public ObservableField<String> getSpend() {
        return mSpend;
    }

    /**
     * 当前充电站距离
     **/
    private ObservableField<String> mDistance;
    public ObservableField<String> getDistance() {
        return mDistance;
    }

    /**
     * 当前充电站快充占用数量
     **/
    private ObservableField<String> mFastFree;
    public ObservableField<String> getFastFree() {
        return mFastFree;
    }

    /**
     * 当前充电站快充总数量
     **/
    private ObservableField<String> mFastTotal;
    public ObservableField<String> getFastTotal() {
        return mFastTotal;
    }

    /**
     * 当前充电站慢充占用数量
     **/
    private ObservableField<String> mSlowFree;
    public ObservableField<String> getSlowFree() {
        return mSlowFree;
    }

    /**
     * 当前充电站慢充总数量
     **/
    private ObservableField<String> mSlowTotal;
    public ObservableField<String> getSlowTotal() {
        return mSlowTotal;
    }

    public BaseAlterChargeViewModel(final @NonNull Application application) {
        super(application);
        mShowAlterCharge = new ObservableField<>(true);
        mRouteSearchStatus = new ObservableField<>("");
        mRouteSearchName = new ObservableField<>("");
        mRouteSearchAddress = new ObservableField<>("");
        mRouteSearchTimeAndDistance = new ObservableField<>("");
        mRouteSearchElec = new ObservableField<>("");
        mRouteSearchTypeVisibility = new ObservableField<>(0);
        mRouteSearchDetailAddRemoveVia = new ObservableField<>("");
        mSpend = new ObservableField<>(mApplication.getString(R.string.route_invalid));
        mDistance = new ObservableField<>(mApplication.getString(R.string.route_invalid));
        mFastFree = new ObservableField<>(mApplication.getString(R.string.route_invalid));
        mFastTotal = new ObservableField<>(mApplication.getString(R.string.route_invalid));
        mSlowFree = new ObservableField<>(mApplication.getString(R.string.route_invalid));
        mSlowTotal = new ObservableField<>(mApplication.getString(R.string.route_invalid));
    }

    @Override
    protected AlterChargeModel initModel() {
        return new AlterChargeModel();
    }

    /**
     * 请求替换充电站信息
     * @param poiId poiId
     */
    public void requestAlterChargeStation(final String poiId) {
        mModel.requestAlterChargeStation(poiId);
    }

    /**
     * 显示充电站列表信息
     * @param routeAlterChargeStationParam 替换充电站搜索信息
     */
    public void showAlterChargeStationInfo(final RouteAlterChargeStationParam routeAlterChargeStationParam) {
        mView.showAlterChargeStationInfo(routeAlterChargeStationParam);
    }

    /**
     * 请求充电站详情信息
     * @param poiId poiID
     */
    public void getSearchDetailsMode(final String poiId) {
        mModel.getSearchDetailsMode(poiId);
    }

    /**
     * 请求当前充电站详情信息
     * @param poiId poiID
     */
    public void getCurrentDetails(final String poiId) {
        mModel.getCurrentDetails(poiId);
    }

    /**
     * 添加途径点
     * @param info 替换充电站信息
     */
    public void addViaList(final RouteAlterChargeStationInfo info) {
        mModel.addViaList(info);
    }

    /**
     * 添加途径点
     * @param poiInfoEntities 点信息
     */
    public void addViaList(final PoiInfoEntity poiInfoEntities) {
        mModel.addViaList(poiInfoEntities);
    }

    /**
     * 显示替换充电站详情界面
     * @param poiInfoEntities 点参数
     */
    public void showChargeStationDetail(final PoiInfoEntity poiInfoEntities) {
        mRouteSearchElec.set("20%");
        mView.showChargeStationDetail(poiInfoEntities);
        mModel.getTravelTimeFuture(new GeoPoint(poiInfoEntities.getPoint().getLon(),poiInfoEntities.getPoint().getLat()))
                .thenAccept(pair -> {
                    mRouteSearchTimeAndDistance.set(MessageFormat.format("{0}  {1}", pair.first, pair.second));
                })
                .exceptionally(error -> {
                    Logger.d(TAG, "getTravelTimeFuture error:" + error);
                    return null;
                });
    }

    /**
     * 显示当前充电站数据
     * @param poiInfoEntities 点参数
     */
    public void showCurrentChargeStation(final PoiInfoEntity poiInfoEntities) {
        if (poiInfoEntities.getMChargeInfoList() == null) {
            return;
        }
        final ChargeInfo chargeInfo = poiInfoEntities.getMChargeInfoList().get(0);
        if (chargeInfo.getCurrentElePrice() != null && !chargeInfo.getCurrentElePrice().isEmpty()) {
            final double price = Double.parseDouble(chargeInfo.getCurrentElePrice());
            mSpend.set(String.format("%.2f", price) + mApplication.getString(R.string.route_details_charge_free_unit));
        }
        mFastFree.set(String.valueOf(chargeInfo.getFast_free()));
        mFastTotal.set(mApplication.getString(R.string.route_details_jg) + chargeInfo.getFast_total());
        mSlowFree.set(String.valueOf(chargeInfo.getSlow_free()));
        mSlowTotal.set(mApplication.getString(R.string.route_details_jg) + chargeInfo.getSlow_total());
        mModel.getTravelTimeFuture(new GeoPoint(poiInfoEntities.getPoint().getLon(),poiInfoEntities.getPoint().getLat()))
                .thenAccept(pair -> {
                    mDistance.set(pair.first);
                })
                .exceptionally(error -> {
                    Logger.d(TAG, "getTravelTimeFuture error:" + error);
                    return null;
                });
    }

    // 防止点击穿透
    private final Action mRootClick = () -> {
    };

    public Action getRootClick() {
        return mRootClick;
    }

    private final Action mClosePage = () -> {
        StackManager.getInstance().getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name()).closeFragment(true);
    };

    public Action getClosePage() {
        return mClosePage;
    }

    private final Action mCloseDetail = () -> {
        mShowAlterCharge.set(true);
    };

    public Action getCloseDetail() {
        return mCloseDetail;
    }
}
