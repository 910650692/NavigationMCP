
package com.fy.navi.scene.ui.poi;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryParam;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.adapter.GasStationAdapter;
import com.fy.navi.scene.adapter.GridSpacingItemDecoration;
import com.fy.navi.scene.databinding.ScenePoiDetailsContentViewBinding;
import com.fy.navi.scene.impl.poi.ScenePoiDetailContentViewImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.PoiDetailsScenicChildAdapter;
import com.fy.navi.scene.ui.adapter.RoutePOIGasStationAdapter;
import com.fy.navi.scene.ui.search.SearchConfirmDialog;
import com.fy.navi.scene.ui.search.SearchLoadingDialog;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import org.json.JSONException;
import org.json.JSONObject;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * @author baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 * @version \$Revision1.0\$
 */
public class ScenePoiDetailContentView extends BaseSceneView<ScenePoiDetailsContentViewBinding,
        ScenePoiDetailContentViewImpl> {
    private static final String HOME_COMPANY_FRAGMENT = "HomeCompanyFragment";
    private static final String DEFATULE_STRING = "--";
    private static final String DIVIDER = "_";
    private PoiInfoEntity mPoiInfoEntity;
    private SearchLoadingDialog mSearchLoadingDialog;
    private final int mSpacing = 24; // 上下间距
    private final int mHorizontalSpacing = 32; // 左右间距
    private final int mChildSpacing = 24;//子POI info item间距
    private final int mSpanCount = 2;//数据列数
    private PoiInfoEntity mChildSelectInfo;
    private int mPoiType;

    public ScenePoiDetailContentView(final @NonNull Context context) {
        super(context);
    }

    public ScenePoiDetailContentView(final @NonNull Context context,
                                     final @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public ScenePoiDetailContentView(final @NonNull Context context,
                                     final @Nullable AttributeSet attrs,
                                     final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected ScenePoiDetailsContentViewBinding createViewBinding(final LayoutInflater inflater,
                                                                  final ViewGroup viewGroup) {
        return ScenePoiDetailsContentViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected ScenePoiDetailContentViewImpl initSceneImpl() {
        return new ScenePoiDetailContentViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScenePoiDetailContentView(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        intSearchLoadingDialog();

        mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setOnClickListener(v ->
                handleRouteClick());
        mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setOnClickListener(v ->
                handleAroundSearchClick());
        mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setOnClickListener(v ->
                handleFavoriteClick());

        updateRouteButton();
    }

    /**
     * 去这里按钮的点击事件
     */

    private void handleRouteClick() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "点击去这里");
        if (mChildSelectInfo != null) {
            if (SearchPackage.getInstance().isAlongWaySearch()) {
                RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                        mChildSelectInfo);
            } else {
                openRouteFragment(mChildSelectInfo);
            }
        }else {
            if (SearchPackage.getInstance().isAlongWaySearch()) {
                RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                        mPoiInfoEntity);
            } else {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "end point1: " + mPoiInfoEntity.getPoint().getLon()
                            + " ,lat" + mPoiInfoEntity.getPoint().getLat());
                openRouteFragment(mPoiInfoEntity);
            }
        }

    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_GO)
    private void openRouteFragment(PoiInfoEntity poiInfoEntity) {
        final Fragment fragment = (Fragment) ARouter.getInstance().build(
                RoutePath.Route.ROUTE_FRAGMENT).navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));

        //for burying point
        JSONObject params = new JSONObject();
        try {
            params.put(BuryConstant.Key.ROUTE_POI_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
            params.put(BuryConstant.Key.POI_INFO_ENTRY, poiInfoEntity);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        BuryProperty properties = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, params.toString())
                .build();
        BuryPointController.getInstance().setBuryProps(properties);
    }

    /**
     * 周边搜索按钮的点击事件
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_NEARBY)
    private void handleAroundSearchClick() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "点击周边搜索");
        final Fragment fragment = (Fragment) ARouter.getInstance().build(
                RoutePath.Search.AROUND_SEARCH_FRAGMENT).navigation();
        addFragment((BaseFragment) fragment,
                SearchFragmentFactory.createAroundFragment(mPoiInfoEntity));

        //for burying point
        BuryProperty properties = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, mPoiInfoEntity.getName())
                .build();
        BuryPointController.getInstance().setBuryProps(properties);
    }

    /**
     * 收藏按钮的点击事件
     */
    private void handleFavoriteClick() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "点击收藏");
        if (mPoiInfoEntity != null) {
            SettingUpdateObservable.getInstance().onUpdateSyncTime();

            final boolean isFavorite = !mScreenViewModel.isFavorite(mPoiInfoEntity).isEmpty();

            final int favoriteIcon = isFavorite ? R.drawable.icon_basic_ic_star_default :
                    R.drawable.img_basic_ic_star;
            mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(
                    ContextCompat.getDrawable(getContext(), favoriteIcon));

            if (isFavorite) {
                mScreenViewModel.removeFavorite(mPoiInfoEntity);
//                mScreenViewModel.deleteFavoriteData(mPoiInfoEntity.getFavoriteInfo().getItemId());
                ToastUtils.Companion.getInstance().showCustomToastView("取消收藏");
            } else {
                mScreenViewModel.addFavorite(mPoiInfoEntity, 0);
//                mScreenViewModel.addFavoriteData(mPoiInfoEntity, 0);
                ToastUtils.Companion.getInstance().showCustomToastView("收藏成功");
            }
        }
    }

    /**
     * 添加途径点按钮点击事件
     */
    private void updateRouteButton() {
        final boolean isAlongWaySearch = mScreenViewModel.isAlongWaySearch();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, isAlongWaySearch ? "添加途径点" : "去这里");

        final int routeIcon = isAlongWaySearch ? R.drawable.img_basic_ic_add :
                R.drawable.icon_details_bottom_go_here;
        final int routeText = isAlongWaySearch ? R.string.st_along_way_point : R.string.st_go_here;

        mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                ContextCompat.getDrawable(getContext(), routeIcon));
        mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(routeText);
    }


    /**
     * 初始化加载弹窗
     */
    private void intSearchLoadingDialog() {
        mSearchLoadingDialog = new SearchLoadingDialog(getContext());
    }

    /**
     * 搜索结果回调
     * @param searchResultEntity 数据实体类
     */
    public void onSearchResult(final SearchResultEntity searchResultEntity) {
        if (null == searchResultEntity || searchResultEntity.getPoiList().isEmpty()) {
            //ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            return;
        }
        if (null != mSearchLoadingDialog) {
            mSearchLoadingDialog.hide();
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiType: " + searchResultEntity.getPoiType());
        if (searchResultEntity.getPoiType() == 0) {
            final CityDataInfo cityDataInfo = mScreenViewModel.getCityInfo(mScreenViewModel.getAcCode());
            if (cityDataInfo != null) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "城市数据信息: " + cityDataInfo.getName() +"，城市编码: "
                        + mScreenViewModel.getAcCode());
                mViewBinding.poiOfflineHint.setVisibility(VISIBLE);
                mViewBinding.poiOfflineHint.setText(
                        getContext().getString(R.string.search_offline_hint, cityDataInfo.getName()));
            }
        }
        this.mPoiInfoEntity = searchResultEntity.getPoiList().get(0);
        initNormalView();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiAoiBounds is: " + mPoiInfoEntity.getMPoiAoiBounds());
        if (mPoiInfoEntity != null && mScreenViewModel != null) {
            ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
            final int pointTypeCode = mScreenViewModel.getPointTypeCode(mPoiInfoEntity.getPointTypeCode());
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "pointTypeCode is: " + pointTypeCode);
            switch (pointTypeCode) {
                case AutoMapConstant.PointTypeCode.GAS_STATION:
                    refreshGasStationView();
                    break;
                case AutoMapConstant.PointTypeCode.CHARGING_STATION:
                    refreshChargeStationView();
                    break;
                case AutoMapConstant.PointTypeCode.CAR_WASH:
                    refreshCarWashView();
                    break;
                case AutoMapConstant.PointTypeCode.CATERING:
                    refreshCateringView();
                    break;
                case AutoMapConstant.PointTypeCode.PARKING_LOT:
                    refreshParkingLotView();
                    break;
                case AutoMapConstant.PointTypeCode.SERVICE_AREA:
                    refreshServiceAreaView();
                    break;
                case AutoMapConstant.PointTypeCode.SCENIC_SPOT:
                    refreshScenicSpotView();
                    break;
                case AutoMapConstant.PointTypeCode.OTHERS:
                default:
                    refreshNormalView();
                    break;
            }
        }
    }

    /**
     * 刷新ETA信息 距离，到达时间，预计剩余电量
     */
    private void refreshEtaInfoView() {
        mScreenViewModel.getTravelTimeFuture(new GeoPoint(mPoiInfoEntity.getPoint().getLon(),
                        mPoiInfoEntity.getPoint().getLat()))
                .thenAccept(etaInfo -> {
                    ThreadManager.getInstance().postUi(new Runnable() {
                        @Override
                        public void run() {
                            if (!ConvertUtils.isEmpty(etaInfo) && !ConvertUtils.isEmpty(mViewBinding)
                                    && !ConvertUtils.isEmpty(mViewBinding.poiDistanceTime)) {
                                final String distance = formatDistanceArrayInternal(
                                        etaInfo.getDistance());
                                mViewBinding.poiDistanceTime.setText(MessageFormat.format("{0} {1}",
                                        distance, etaInfo.getTravelTime()));
                                final int leftCharge = Math.max(-99, etaInfo.getLeftCharge());
                                if (!ConvertUtils.isEmpty(leftCharge)) {
                                    //50%以上电量，显示满电量图片，20-50%电量，显示半电量图片
                                    //0-20电量，显示低电量图片，文本变红
                                    //小于0%电量，显示空电量图片，文本变红
                                    if (leftCharge >= 50 && leftCharge <= 100) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_full_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.poi_details_bottom_ff_00));
                                    } else if (leftCharge > 20 && leftCharge < 50) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_medium_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.poi_details_bottom_ff_00));
                                    } else if (leftCharge > 0 && leftCharge <= 20) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_low_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.navi_color_C73333_100));
                                    } else if (leftCharge <= 0) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_empty_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.navi_color_C73333_100));
                                    }
                                }
                                mViewBinding.poiArrivalCapacity.setText(getContext().getString(
                                        R.string.remain_charge, leftCharge));
                            }
                        }
                    });

                })
                .exceptionally(error -> {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "getTravelTimeFuture error:" + error);
                    return null;
                });
    }

    /**
     * 刷新通用视图
     */
    private void initNormalView() {
        if (mViewBinding != null && mPoiInfoEntity != null) {
            mViewBinding.skPoiName.setText(mPoiInfoEntity.getName());
            mViewBinding.poiSecondAddress.setText(mPoiInfoEntity.getAddress());
            if (ConvertUtils.isEmpty(mPoiInfoEntity.getBusinessTime())) {
                mViewBinding.scenePoiDetailsNormalView.poiBusinessHours.setVisibility(View.GONE);
            } else {
                mViewBinding.scenePoiDetailsNormalView.poiBusinessHours.setText(
                        getContext().getString(R.string.business_hour, mPoiInfoEntity.getBusinessTime()));
            }
            if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
                mViewBinding.scenePoiDetailsNormalView.poiPhone.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPhone.setVisibility(View.GONE);
            }
            mViewBinding.scenePoiDetailsNormalView.poiPhone.setText(
                    getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()));

            mViewBinding.scenePoiDetailsBottomView.stlPhone.setOnClickListener(new OnClickListener() {
                @Override
                @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_PHONE)
                public void onClick(View v) {
                    final String phone = mPoiInfoEntity.getPhone();
                    final List<String> phoneString = new ArrayList<>();
                    StringBuffer phoneProp = new StringBuffer();
                    if (phone.contains(";")) {
                        final String[] split = phone.split(";");
                        phoneString.addAll(Arrays.asList(split));
                    } else {
                        phoneString.add(phone);
                    }
                    if (!ConvertUtils.isEmpty(phoneString) && !ConvertUtils.isEmpty(phoneString.get(0))) {
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "call phone: " + phoneString.get(0));
                        phoneProp.append(phoneString.get(0));
                        new SearchConfirmDialog.Build(getContext())
                                .setDialogObserver(new IBaseDialogClickListener() {
                                    @Override
                                    public void onCommitClick() {
                                        //拨打电话
                                        final Intent intent = new Intent();
                                        intent.setAction(Intent.ACTION_CALL);
                                        intent.setData(Uri.parse("tel:" + phoneString.get(0)));
                                        final Context context = getContext();
                                        context.startActivity(intent);
                                    }

                                    @Override
                                    public void onCancelClick() {

                                    }
                                })
                                .setContent(getContext().getString(R.string.text_dial_phone_content, phoneString.get(0)))
                                .setConfirmTitle(getContext().getString(R.string.text_dial))
                                .build().show();

                    } else {
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "call phone is null ");
                        phoneProp.append("");
                    }

                    //For burying point
                    BuryProperty buryProperty = new BuryProperty.Builder()
                            .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, phoneProp.toString())
                            .build();
                    BuryPointController.getInstance().setBuryProps(buryProperty);

                }
            });
            final FavoriteInfo favoriteInfo = new FavoriteInfo()
                    .setCommonName(0)
                    .setUpdateTime(new Date().getTime());

            final String itemId = mScreenViewModel.isFavorite(mPoiInfoEntity);
            mPoiInfoEntity.setFavoriteInfo(favoriteInfo);
            if (!itemId.isEmpty()) {
                mPoiInfoEntity.getFavoriteInfo().setItemId(itemId);
                mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(
                        ContextCompat.getDrawable(getContext(), R.drawable.img_basic_ic_star));
            } else {
                mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(
                        ContextCompat.getDrawable(getContext(),
                                R.drawable.icon_basic_ic_star_default));
            }
            refreshEtaInfoView();
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "mViewBinding or poiInfoEntity is null");
        }
    }

    /**
     * 刷新加油站视图
     */
    private void refreshGasStationView() {
        final List<GasStationInfo> gasStationInfos = mPoiInfoEntity.getStationList();
        for (GasStationInfo gasStationInfo : gasStationInfos) {
            gasStationInfo.setPrice(getContext().getString(R.string.oil_price, gasStationInfo.getPrice()));
        }
        final GasStationAdapter gasStationAdapter = new GasStationAdapter();
        gasStationAdapter.setGasStationList(gasStationInfos);
        mViewBinding.scenePoiDetailsGasStationView.poiGasBusinessHours.
                setText(getContext().getString(R.string.business_hour, mPoiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsGasStationView.poiGasPhone.setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsGasStationView.poiGasPhone.setText(
                getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()));
        mViewBinding.scenePoiDetailsGasStationView.poiGasOilList.setLayoutManager(
                new GridLayoutManager(getContext(), mSpanCount));
        mViewBinding.scenePoiDetailsGasStationView.poiGasOilList.addItemDecoration(
                new GridSpacingItemDecoration(getContext(), mSpanCount, mSpacing, mHorizontalSpacing,
                        false));
        mViewBinding.scenePoiDetailsGasStationView.poiGasOilList.setAdapter(gasStationAdapter);
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新充电桩视图
     */
    private void refreshChargeStationView() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "imageUrl is: " + mPoiInfoEntity.getImageUrl());
        final List<ChargeInfo> chargeInfos = mPoiInfoEntity.getChargeInfoList();
        final ChargeInfo chargeInfo = chargeInfos.get(0);
        if (chargeInfo.getSlowVolt() == 0 && chargeInfo.getSlowPower() == 0
                && chargeInfo.getSlow_free() == 0 && chargeInfo.getSlow_total() == 0) {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowLayout.setVisibility(GONE);
        } else {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowLayout.
                    setVisibility(VISIBLE);
        }
        if (chargeInfo.getFastVolt() == 0 && chargeInfo.getFastPower() == 0
                && chargeInfo.getFast_free() == 0 && chargeInfo.getFast_total() == 0) {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastLayout.setVisibility(GONE);
        } else {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastLayout.
                    setVisibility(VISIBLE);
        }
        final String fastFree = chargeInfo.getFast_free() == 0 ?
                DEFATULE_STRING : chargeInfo.getFast_free() + "";
        final String fastTotal = chargeInfo.getFast_total() == 0 ?
                DEFATULE_STRING : "/" + chargeInfo.getFast_total();
        final String fastVolt = chargeInfo.getFastVolt() == 0 ?
                DEFATULE_STRING : chargeInfo.getFastVolt() + "v";
        final String fastPower = chargeInfo.getFastPower() == 0 ?
                DEFATULE_STRING : chargeInfo.getFastPower() + "kw";
        final String fastInfo = fastPower + "." + fastVolt;
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastOccupied.setText(fastFree);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastTotal.setText(fastTotal);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastCurrentAndVlot.
                setText(fastInfo);
        final String slowFree = chargeInfo.getSlow_free() == 0 ?
                DEFATULE_STRING : chargeInfo.getSlow_free() + "";
        final String slowTotal = chargeInfo.getSlow_total() == 0 ?
                DEFATULE_STRING : "/" + chargeInfo.getSlow_total();
        final String slowVolt = chargeInfo.getSlowVolt() == 0 ?
                DEFATULE_STRING : chargeInfo.getSlowVolt() + "v";
        final String slowPower = chargeInfo.getSlowPower() == 0 ?
                DEFATULE_STRING : chargeInfo.getSlowPower() + "kw";
        final String slowInfo = slowPower + "." + slowVolt;
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowOccupied.setText(slowFree);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowTotal.setText(slowTotal);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot.
                setText(slowInfo);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargePrice.setText(
                getContext().getString(
                        R.string.charge_price, chargeInfo.getCurrentElePrice()));
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice.setText(
                getContext().getString(
                        R.string.charge_park_price, chargeInfo.getCurrentServicePrice()));
        mViewBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours.setText(
                getContext().getString(R.string.business_hour, mPoiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.
                    setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setText(
                getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()));
        final String imageUrl = mPoiInfoEntity.getImageUrl();
        ViewAdapterKt.loadImageUrl(mViewBinding.scenePoiDetailsChargingStationView.poiChargeImg,
                imageUrl, R.drawable.test_pic, R.drawable.test_pic);
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新洗车视图
     */
    private void refreshCarWashView() {
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新餐饮信息
     */
    private void refreshCateringView() {
        final String rating = mPoiInfoEntity.getRating();
        if (TextUtils.isEmpty(rating)) {
            mViewBinding.scenePoiDetailsCateringView.poiCateringMarkValue.setVisibility(GONE);
            mViewBinding.scenePoiDetailsCateringView.poiCateringMarkLayout.setVisibility(GONE);
        } else {
            mViewBinding.scenePoiDetailsCateringView.poiCateringMarkValue.setText(rating);
            final float realRating = Float.parseFloat(rating);
            if (realRating >= 0.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg1.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 0.5 && realRating > 0) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg1.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg1.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 1.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg2.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 1.5 && realRating > 1) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg2.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg2.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 2.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg3.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 2.5 && realRating > 2) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg3.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg3.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 3.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg4.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 3.5 && realRating > 3) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg4.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg4.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 4.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg5.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 4.5 && realRating > 4) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg5.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg5.setImageResource(
                        R.drawable.img_collect3_42);
            }
        }
        final String avgCost;
        if (mPoiInfoEntity.getAverageCost() == -1) {
            avgCost = DEFATULE_STRING;
            mViewBinding.scenePoiDetailsCateringView.poiCateringPricePerPerson.setVisibility(GONE);
        } else {
            avgCost = getContext().getString(R.string.catering_price, mPoiInfoEntity.getAverageCost());
            mViewBinding.scenePoiDetailsCateringView.poiCateringPricePerPerson.
                    setVisibility(VISIBLE);
        }
        mViewBinding.scenePoiDetailsCateringView.poiCateringHours.setText(
                getContext().getString(R.string.business_hour, mPoiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsCateringView.poiCateringPhone.setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsCateringView.poiCateringPhone.setText(
                getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()));
        mViewBinding.scenePoiDetailsCateringView.poiCateringPrice.setText(avgCost);
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新停车场视图
     */
    private void refreshParkingLotView() {
        if (mPoiInfoEntity == null || mPoiInfoEntity.getParkingInfoList() == null
                || mPoiInfoEntity.getParkingInfoList().isEmpty()) {
            return;
        }
        final ParkingInfo parkingInfo = mPoiInfoEntity.getParkingInfoList().get(0);
        String parkString = "";
        final int spaceFree = parkingInfo.getSpaceFree();
        final int spaceTotal = parkingInfo.getSpaceTotal();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "spaceFree :" + spaceFree + " spaceTotal :" + spaceTotal);
        if (spaceFree == -1 && spaceTotal == -1) {
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setVisibility(GONE);
        } else if (spaceFree == -1) {
            parkString = getContext().getString(R.string.parking_lot_total, spaceTotal);
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setText(parkString);
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.search_loading_bg_80));
        } else {
            parkString = getContext().getString(R.string.parking_lot_status, spaceFree, spaceTotal);
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setText(parkString);
        }
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setText(parkString);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotHours.setText(
                getContext().getString(R.string.business_hour, mPoiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotPhone.setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotPhone.setText(
                getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()));
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新服务区视图
     */
    private void refreshServiceAreaView() {
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility.setVisibility(GONE);
        final List<GasStationInfo> gasStationInfos = mPoiInfoEntity.getStationList();
        if (gasStationInfos == null || gasStationInfos.isEmpty()) {
            mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaOil.setVisibility(GONE);
        } else {
            final LinearLayoutManager layoutManager1 = new LinearLayoutManager(getContext());
            layoutManager1.setOrientation(LinearLayoutManager.HORIZONTAL);
            mViewBinding.scenePoiDetailsServiceAreaView.routePoidetailGasStation.
                    setLayoutManager(layoutManager1);
            final RoutePOIGasStationAdapter serviceGasAdapter = new RoutePOIGasStationAdapter();
            mViewBinding.scenePoiDetailsServiceAreaView.routePoidetailGasStation.setAdapter(
                    serviceGasAdapter);
            final List<String> gasString = new ArrayList<>();
            for (GasStationInfo gasStationInfo : gasStationInfos) {
                gasString.add(gasStationInfo.getType());
            }
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "gasString = 1" + gasString + " size: "
                    + gasStationInfos.size());
            serviceGasAdapter.setRouteBeanList(gasString);
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getBusinessTime())) {
            mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours.setVisibility(View.GONE);
        } else {
            mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours.setText(
                    getContext().getString(R.string.business_hour, mPoiInfoEntity.getBusinessTime()));
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.
                    setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setText(
                getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()));
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新子项列表
     */
    private void refreshChildListView() {
        final List<ChildInfo> childInfoList = mPoiInfoEntity.getChildInfoList();
        final PoiDetailsScenicChildAdapter scenicChildAdapter = new PoiDetailsScenicChildAdapter();
        if (childInfoList != null && !childInfoList.isEmpty()) {
            scenicChildAdapter.setChildInfoList(childInfoList);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.setLayoutManager(
                    new GridLayoutManager(getContext(), mSpanCount));
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.addItemDecoration(
                    new GridSpacingItemDecoration(getContext(), mSpanCount, mChildSpacing,
                            mChildSpacing, false));
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.
                    setAdapter(scenicChildAdapter);
            if (childInfoList.size() > 2) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                        setVisibility(VISIBLE);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                        setVisibility(GONE);
            }
            scenicChildAdapter.setItemClickListener((index, isSelectIndex) -> {
                final ChildInfo childInfo = childInfoList.get(index);
                mChildSelectInfo = new PoiInfoEntity()
                        .setName(childInfo.getName())
                        .setAddress(childInfo.getAddress())
                        .setPid(childInfo.getPoiId())
                        .setPoint(childInfo.getLocation());
            });
        } else {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                    setVisibility(GONE);
        }
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                setOnClickListener(v -> {
                    scenicChildAdapter.setCollapse(!scenicChildAdapter.isCollapse());
                    scenicChildAdapter.notifyDataSetChanged();
                    mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                            setImageResource(scenicChildAdapter.isCollapse() ?
                                    R.drawable.img_under_the_48 : R.drawable.img_up_48);
                });
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                setImageResource(scenicChildAdapter.isCollapse() ? R.drawable.img_under_the_48 :
                        R.drawable.img_up_48);
    }

    /**
     * 刷新景点视图
     */
    private void refreshScenicSpotView() {
        refreshChildListView();
        final String hourTime =  mPoiInfoEntity.getBusinessTime();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "hourTime : " + hourTime);

        final String rating = mPoiInfoEntity.getRating();
        if (TextUtils.isEmpty(rating)) {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkLayout.setVisibility(GONE);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkValue.setVisibility(GONE);
        } else {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkValue.setText(rating);
            final float realRating = Float.parseFloat(rating);
            if (realRating >= 0.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg1.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 0.5 && realRating > 0){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg1.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg1.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 1.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg2.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 1.5 && realRating > 1){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg2.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg2.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 2.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg3.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 2.5 && realRating > 2){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg3.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg3.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 3.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg4.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 3.5 && realRating > 3){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg4.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg4.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 4.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg5.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 4.5 && realRating > 4){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg5.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg5.setImageResource(
                        R.drawable.img_collect3_42);
            }
        }
        final String avgCost;
        if (mPoiInfoEntity.getAverageCost() == -1) {
            avgCost = DEFATULE_STRING;
        } else {
            avgCost = getContext().getString(R.string.catering_price, mPoiInfoEntity.getAverageCost());
        }

        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotHoursContent.setText(
                getContext().getString(R.string.business_hour, mPoiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPhone.setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPhone.setText(
                getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()));
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPrice.setText(avgCost);
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新通用视图
     */
    private void refreshNormalView() {
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(VISIBLE);
        if (mPoiInfoEntity == null) {
            return;
        }
        final List<ChildInfo> childInfoList = mPoiInfoEntity.getChildInfoList();
        final PoiDetailsScenicChildAdapter scenicChildAdapter = new PoiDetailsScenicChildAdapter();
        if (childInfoList != null && !childInfoList.isEmpty()) {
            scenicChildAdapter.setChildInfoList(childInfoList);
            mViewBinding.scenePoiDetailsNormalView.poiChildList.setLayoutManager(
                    new GridLayoutManager(getContext(), mSpanCount));
            mViewBinding.scenePoiDetailsNormalView.poiChildList.addItemDecoration(
                    new GridSpacingItemDecoration(getContext(), mSpanCount, mChildSpacing,
                            mChildSpacing, false));
            mViewBinding.scenePoiDetailsNormalView.poiChildList.setAdapter(scenicChildAdapter);
            if (childInfoList.size() > 2) {
                mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.
                        setVisibility(VISIBLE);
            } else {
                mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setVisibility(GONE);
            }
            scenicChildAdapter.setItemClickListener((index, isSelectIndex) -> {
                final ChildInfo childInfo = childInfoList.get(index);
                mChildSelectInfo = new PoiInfoEntity()
                        .setName(childInfo.getName())
                        .setAddress(childInfo.getAddress())
                        .setPid(childInfo.getPoiId())
                        .setPoint(childInfo.getLocation());
            });
        } else {
            mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setVisibility(GONE);
        }
        mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setOnClickListener(v -> {
            scenicChildAdapter.setCollapse(!scenicChildAdapter.isCollapse());
            scenicChildAdapter.notifyDataSetChanged();
            mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setImageResource(
                    scenicChildAdapter.isCollapse() ?
                            R.drawable.img_under_the_48 : R.drawable.img_up_48);
        });
        mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setImageResource(
                scenicChildAdapter.isCollapse() ?
                        R.drawable.img_under_the_48 : R.drawable.img_up_48);

    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mChildSelectInfo = null;
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onDestroy");
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
    }

    private final Runnable mTimeoutTask = new Runnable() {
        @Override
        public void run() {
            if (null != mSearchLoadingDialog) {
                mSearchLoadingDialog.hide();
                if (!ConvertUtils.isEmpty(mViewBinding)) {
                    mViewBinding.csPoiNoResult.setVisibility(View.VISIBLE);
                    mViewBinding.skPoiName.setVisibility(View.GONE);
                    mViewBinding.poiDetailsScroll.setVisibility(View.GONE);
                    mViewBinding.scenePoiDetailsBottomView.getRoot().setVisibility(View.GONE);

                    mViewBinding.noResultButton.setOnClickListener((view) -> {
                        doSearch(mPoiInfoEntity);
                        mViewBinding.csPoiNoResult.setVisibility(View.GONE);
                        mViewBinding.skPoiName.setVisibility(View.VISIBLE);
                        mViewBinding.poiDetailsScroll.setVisibility(View.VISIBLE);
                        mViewBinding.scenePoiDetailsBottomView.getRoot().setVisibility(View.VISIBLE);
                    });
                }
            }
        }
    };

    /**
     * 执行搜索操作
     * @param poiInfo 搜索对象实体类
     */
    public void doSearch(final PoiInfoEntity poiInfo) {
        if (null != mSearchLoadingDialog) {
            mSearchLoadingDialog.show();
        }
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        ThreadManager.getInstance().postDelay(mTimeoutTask, 3000);
        mPoiInfoEntity = poiInfo;
        mScreenViewModel.doSearch(poiInfo);
        //todo 目前尚未提供CVP点击接口，暂时使用经纬度实现，后续提供接口后修改实现
        if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CLICK) {
            final GeoPoint currentLocation = mScreenViewModel.getCurrentLocation();
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "当前位置 lon" + currentLocation.getLon() + " , lat"
                    + currentLocation.getLon()
                    + " 目标位置 lon" + poiInfo.getPoint().getLon() + " , lat" + poiInfo.getPoint().getLat());
            if (currentLocation.getLon() == poiInfo.getPoint().getLon()
                    && currentLocation.getLat() == poiInfo.getPoint().getLat()) {
                mViewBinding.poiDistanceTime.setVisibility(View.GONE);
                mViewBinding.poiArrivalCapacity.setVisibility(View.GONE);
                mViewBinding.sivArrivalCapacity.setVisibility(View.GONE);
            }
        }
    }

    /**
     * 刷新poi视图
     *
     * @param poiType poi类型
     *                POI_SUGGESTION = 0; // 预搜索
     *                POI_KEYWORD = 1; // 关键字搜索
     *                POI_HOME = 2; // 添加家
     *                POI_COMPANY = 3; // 添加公司
     *                POI_COLLECTION = 4; // 添加收藏地址
     *                POI_COMMON = 5; // 添加常用地址
     *                POI_AROUND = 6; // 添加途径点
     *                POI_MAP_CLICK = 7; // 地图点击
     */
    public void refreshPoiView(final int poiType) {
        if (mViewBinding == null) {
            return;
        }
        this.mPoiType = poiType;
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiType: " + poiType);
        refreshNormalView();
        //刷新View
        switch (poiType) {
            case AutoMapConstant.PoiType.POI_SUGGESTION:
            case AutoMapConstant.PoiType.POI_KEYWORD:
            case AutoMapConstant.PoiType.POI_MAP_CLICK:
            case AutoMapConstant.PoiType.POI_AROUND:
                if (mScreenViewModel.isAlongWaySearch()) {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "添加途径点");
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_along_way_point);
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(GONE);
                    mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                    if (mScreenViewModel.getViaCount() >= 1) {
                        mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(GONE);
                        mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setVisibility(VISIBLE);
                    } else {
                        mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                    }
                } else {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "去这里");
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                            ContextCompat.getDrawable(getContext(),
                                    R.drawable.icon_details_bottom_go_here));
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_go_here);
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(VISIBLE);
                    mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(VISIBLE);
                    mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                }
                if (poiType == AutoMapConstant.PoiType.POI_MAP_CLICK) {
                    //地图选点不需要展示电话和营业时间界面
                    mViewBinding.poiContentLayout.setVisibility(View.GONE);
                }
                break;
            case AutoMapConstant.PoiType.POI_HOME:
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "设置为家");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_add_home);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                break;
            case AutoMapConstant.PoiType.POI_COMPANY:
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "设置为公司");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_add_company);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                break;
            case AutoMapConstant.PoiType.POI_COLLECTION:
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "添加");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                        ContextCompat.getDrawable(getContext(),
                                R.drawable.icon_details_bottom_go_here));
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_collect_add);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.VISIBLE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(GONE);
                break;
            case AutoMapConstant.PoiType.POI_COMMON:
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "添加");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_collect_add);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                break;
            case AutoMapConstant.PoiType.POI_DELETE_AROUND:
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "删除途径点");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_along_way_point_delete);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                break;
            default:
                break;
        }
        //注册点击事件
        registerClickEvent(poiType);
    }

    /**
     * 注册界面点击事件
     * @param poiType POI详情页面类型
     */
    private void registerClickEvent(final int poiType) {
        mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onClick poiType: " + poiType);
            switch (poiType) {
                case AutoMapConstant.PoiType.POI_SUGGESTION:
                case AutoMapConstant.PoiType.POI_KEYWORD:
                case AutoMapConstant.PoiType.POI_AROUND:
                case AutoMapConstant.PoiType.POI_MAP_CLICK:
                    handleRouteClick();

                    break;
                case AutoMapConstant.PoiType.POI_HOME:
                case AutoMapConstant.PoiType.POI_COMPANY:
                case AutoMapConstant.PoiType.POI_COLLECTION:
                case AutoMapConstant.PoiType.POI_COMMON:
                    //  1，家  2，公司 3，常去地址  0，普通收藏点;
                    if (null != mPoiInfoEntity) {
                        final int commonName;
                        final String resultText = switch (poiType) {
                            case AutoMapConstant.PoiType.POI_HOME -> {
                                commonName = 1;
                                closeAllFragmentsUntilTargetFragment(HOME_COMPANY_FRAGMENT);
                                showCurrentFragment();
                                yield "设置家成功";
                            }
                            case AutoMapConstant.PoiType.POI_COMPANY -> {
                                commonName = 2;
                                closeAllFragmentsUntilTargetFragment(HOME_COMPANY_FRAGMENT);
                                showCurrentFragment();
                                yield "设置公司成功";
                            }
                            case AutoMapConstant.PoiType.POI_COMMON -> {
                                commonName = 3;
                                closeAllFragmentsUntilTargetFragment(HOME_COMPANY_FRAGMENT);
                                showCurrentFragment();
                                yield "添加成功";
                            }
                            default -> {
                                commonName = 0;
                                closeAllFragmentsUntilTargetFragment(HOME_COMPANY_FRAGMENT);
                                showCurrentFragment();
                                yield "添加成功";
                            }
                        };
                        final FavoriteInfo favoriteInfo = new FavoriteInfo();
                        favoriteInfo.setCommonName(commonName)
                                .setUpdateTime(new Date().getTime());
                        mPoiInfoEntity.setFavoriteInfo(favoriteInfo);
                        mScreenViewModel.addFavorite(mPoiInfoEntity, commonName);
//                        BehaviorPackage.getInstance().addFavoriteData(mPoiInfoEntity, commonName);
                        SettingUpdateObservable.getInstance().onUpdateSyncTime();
                        ToastUtils.Companion.getInstance().showCustomToastView(resultText);
                    }
                    break;
                case AutoMapConstant.PoiType.POI_DELETE_AROUND:
                    RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP,
                            mPoiInfoEntity, true);
                    break;
                default:
                    break;
            }
        });
        mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setOnClickListener((view) -> {
            if (mChildSelectInfo != null) {
                if (SearchPackage.getInstance().isAlongWaySearch()) {
                    RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                            mChildSelectInfo, 0);
                }
            } else {
                if (SearchPackage.getInstance().isAlongWaySearch()) {
                    RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                            mPoiInfoEntity, 0);
                }
            }
        });
    }

    /**
     * 格式化距离数组
     * @param distance 距离数据
     * @return 格式化后的数据
     */
    private String formatDistanceArrayInternal(final int distance) {
        final String[] distanceArray = ConvertUtils.formatDistanceArray(AppContext.getInstance().getMContext(), distance);
        return distanceArray[0] + distanceArray[1];
    }
}
