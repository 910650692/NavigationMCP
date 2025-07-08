package com.sgm.navi.hmi.route.newAlterCharge;

import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.R;
import com.sgm.navi.scene.ui.search.SearchConfirmDialog;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RouteAlterChargeStationParam;
import com.sgm.navi.service.define.route.RouteSupplementParams;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BaseNewAlterChargeViewModel extends BaseViewModel<NewAlterChargeFragment, NewAlterChargeModel> {
    private static final String TAG = "BaseAlterChargeViewModel";
    private ObservableField<Boolean> mShowAlterCharge;
    /**POI详情页面**/
    private ObservableField<Boolean> mRouteSearchStatusVisibility;
    private ObservableField<Boolean> mRoutePhoneVisibility;
    private ObservableField<String> mRouteSearchStatus;
    private ObservableField<String> mRouteSearchName;
    private ObservableField<String> mRouteSearchAddress;
    private ObservableField<String> mRouteSearchTimeAndDistance;
    private ObservableField<String> mRouteSearchElec;
    private ObservableField<Integer> mRouteSearchTypeVisibility;
    private ObservableField<String> mRouteCurrentName;
    private RouteAlterChargeStationParam mRouteAlterChargeStationParam;

    public ObservableField<Boolean> getShowAlterCharge() {
        return mShowAlterCharge;
    }

    public ObservableField<Boolean> getRoutePhoneVisibility() {
        return mRoutePhoneVisibility;
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


    public ObservableField<String> getRouteCurrentName() {
        return mRouteCurrentName;
    }


    /**
     * 当前充电站距离
     **/
    private ObservableField<String> mDistance;
    public ObservableField<String> getDistance() {
        return mDistance;
    }

    private PoiInfoEntity mDetailsEntry;
    private RouteSupplementParams mCurrentRouteSupplementParams;

    public void setCurrentRouteSupplementParams(RouteSupplementParams mCurrentRouteSupplementParams) {
        this.mCurrentRouteSupplementParams = mCurrentRouteSupplementParams;
    }

    public BaseNewAlterChargeViewModel(final @NonNull Application application) {
        super(application);
        mShowAlterCharge = new ObservableField<>(true);
        mRoutePhoneVisibility = new ObservableField<>(false);
        mRouteSearchStatusVisibility = new ObservableField<>(false);
        mRouteSearchStatus = new ObservableField<>("");
        mRouteSearchName = new ObservableField<>("");
        mRouteSearchAddress = new ObservableField<>("");
        mRouteSearchTimeAndDistance = new ObservableField<>("");
        mRouteSearchElec = new ObservableField<>("");
        mRouteSearchTypeVisibility = new ObservableField<>(0);
        mRouteCurrentName = new ObservableField<>("");
        mDistance = new ObservableField<>(mApplication.getString(R.string.route_invalid));
    }

    @Override
    protected NewAlterChargeModel initModel() {
        return new NewAlterChargeModel();
    }

    /**
     * 请求替换充电站信息
     * @param poiId poiId
     */
    public void requestAlterChargeStation(final String poiId,final int index) {
        mModel.requestAlterChargeStation(poiId, index);
    }

    /**
     * 重置替换充电站信息
     */
    public void clearAlterChargeStation() {
        mModel.clearAlterChargeStation();
    }

    /**
     * 显示充电站列表信息
     * @param routeAlterChargeStationParam 替换充电站搜索信息
     * @param index index
     */
    public void showAlterChargeStationInfo(final RouteAlterChargeStationParam routeAlterChargeStationParam,final int index) {
        mRouteAlterChargeStationParam = routeAlterChargeStationParam;
        mView.showAlterChargeStationInfo(routeAlterChargeStationParam, index);
    }

    /**
     * 恢复充电站列表信息
     */
    public void reStoreFragment() {
        if (mDetailsEntry != null && Boolean.FALSE.equals(getShowAlterCharge().get())) {
            showChargeStationDetail(mDetailsEntry);
        }
        if (mCurrentRouteSupplementParams != null) {
            mView.getSupplementList(mCurrentRouteSupplementParams);
        }
    }

    /**
     * 请求当前充电站详情信息
     * @param poiId poiID
     */
    public void getCurrentDetails(final String poiId) {
        mModel.getCurrentDetails(poiId);
    }

    /**
     * 批量搜索详情信息
     * @param pidList poiID列表
     */
    public void getPoiListSearch(final List<String> pidList) {
        mModel.getPoiListSearch(pidList);
    }

    /**
     * 批量搜索替换详情信息
     * @param pidList poiID列表
     */
    public void getAlterPoiListSearch(final List<String> pidList) {
        mModel.getAlterPoiListSearch(pidList);
    }

    /**
     * 设置批量搜的回调
     * @param poiInfoEntities poi详情数据
     */
    public void setSilentSearchResult(final ArrayList<PoiInfoEntity> poiInfoEntities) {
        mView.setSilentSearchResult(poiInfoEntities);
    }

    /**
     * 设置替换补能点批量搜的回调
     * @param poiInfoEntities poi详情数据
     */
    public void setAlterSilentSearchResult(final ArrayList<PoiInfoEntity> poiInfoEntities) {
        mView.setAlterSilentSearchResult(poiInfoEntities);
    }

    /**
     * 替换补能点
     * @param newPoiInfoEntity 替换点信息
     * @param oldPoiInfoEntity 被替换点信息
     */
    public void replaceSupplement(final PoiInfoEntity newPoiInfoEntity, final PoiInfoEntity oldPoiInfoEntity) {
        mModel.replaceSupplement(newPoiInfoEntity, oldPoiInfoEntity);
    }

    /**
     * 显示替换充电站详情界面
     * @param poiInfoEntities 点参数
     */
    public void showChargeStationDetail(final PoiInfoEntity poiInfoEntities) {
        mDetailsEntry = poiInfoEntities;
        if (!ConvertUtils.isEmpty(mDetailsEntry) && !ConvertUtils.isEmpty(mDetailsEntry.getPhone())) {
            mRoutePhoneVisibility.set(true);
        } else {
            mRoutePhoneVisibility.set(false);
        }
        mView.showChargeStationDetail(poiInfoEntities);
        mModel.getTravelTimeFuture(new GeoPoint(poiInfoEntities.getPoint().getLon(),poiInfoEntities.getPoint().getLat()))
                .thenAccept(pair -> {
                    mRouteSearchTimeAndDistance.set(MessageFormat.format("{0}  {1}", pair.first, pair.second));
                })
                .exceptionally(error -> {
                    Logger.d(TAG, "showChargeStationDetail error:" + error);
                    return null;
                });
        mModel.getTravelTimeFutureWithEv(new GeoPoint(poiInfoEntities.getPoint().getLon(),
                        poiInfoEntities.getPoint().getLat()))
                .thenAccept(etaInfo -> {
                    ThreadManager.getInstance().postUi(() -> {
                        mView.showPOIDetailCharge(etaInfo.getLeftCharge());
                        mRouteSearchElec.set(ResourceUtils.Companion.getInstance().getString(
                                com.sgm.navi.scene.R.string.remain_charge, etaInfo.getLeftCharge()));
                    });

                })
                .exceptionally(error -> {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "showChargeStationDetail error:" + error);
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
        mModel.clearLayerItem();
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

    private final Action mRouteSearchPhoneClick = () -> {
        if (!ConvertUtils.isEmpty(mDetailsEntry) && !ConvertUtils.isEmpty(mDetailsEntry.getPhone())) {
            final String phone = mDetailsEntry.getPhone();
            final List<String> phoneString = new ArrayList<>();
            if (phone.contains(";")) {
                final String[] split = phone.split(";");
                phoneString.addAll(Arrays.asList(split));
            } else {
                phoneString.add(phone);
            }
            if (!ConvertUtils.isEmpty(phoneString) && !ConvertUtils.isEmpty(phoneString.get(0))) {
                Logger.d(TAG, "call phone: " + phoneString.get(0));
                new SearchConfirmDialog.Build(mView.getContext())
                        .setDialogObserver(new IBaseDialogClickListener() {
                            @Override
                            public void onCommitClick() {
                                //拨打电话
                                final Intent intent = new Intent();
                                intent.setAction(Intent.ACTION_CALL);
                                intent.setData(Uri.parse("tel:" + phoneString.get(0)));
                                final Context context = mView.getContext();
                                context.startActivity(intent);
                            }

                            @Override
                            public void onCancelClick() {

                            }
                        })
                        .setContent(mView.getContext().getString(com.sgm.navi.scene.R.string.text_dial_phone_content, phoneString.get(0)))
                        .setConfirmTitle(mView.getContext().getString(com.sgm.navi.scene.R.string.text_dial))
                        .build().show();

            } else {
                Logger.d(TAG, "call phone is null ");
            }
        }
    };
    public Action getRouteSearchPhoneClick() {
        return mRouteSearchPhoneClick;
    }
}
