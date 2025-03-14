
package com.fy.navi.scene.ui.poi;

import static com.fy.navi.service.AutoMapConstant.PoiType;
import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

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
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
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
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
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
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class ScenePoiDetailContentView extends BaseSceneView<ScenePoiDetailsContentViewBinding, ScenePoiDetailContentViewImpl> {
    private PoiInfoEntity poiInfoEntity;
    private SearchLoadingDialog searchLoadingDialog;
    int spacing = 24; // 上下间距
    int horizontalSpacing = 32; // 左右间距
    int childSpaceing = 24;//子POI info item间距
    int spanCount = 2;//数据列数
    private PoiInfoEntity childSelectInfo;
    private int poiType;

    public ScenePoiDetailContentView(@NonNull Context context) {
        super(context);
    }

    public ScenePoiDetailContentView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public ScenePoiDetailContentView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected ScenePoiDetailsContentViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
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

        mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setOnClickListener(v -> handleRouteClick());
        mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setOnClickListener(v -> handleAroundSearchClick());
        mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setOnClickListener(v -> handleFavoriteClick());

        updateRouteButton();
    }

    private void handleRouteClick() {
        Logger.d(SEARCH_HMI_TAG, "点击去这里");
        if (childSelectInfo != null) {
            if (SearchPackage.getInstance().isAlongWaySearch()) {
                RoutePackage.getInstance().addViaPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP, childSelectInfo, RoutePoiType.ROUTE_POI_TYPE_WAY);
            } else {
                Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.ROUTE_FRAGMENT).navigation();
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(childSelectInfo));
            }
        }else {
            if (SearchPackage.getInstance().isAlongWaySearch()) {
                RoutePackage.getInstance().addViaPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
            } else {
                Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.ROUTE_FRAGMENT).navigation();
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
            }
        }

    }

    private void handleAroundSearchClick() {
        Logger.d(SEARCH_HMI_TAG, "点击周边搜索");
        Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Search.AROUND_SEARCH_FRAGMENT).navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createAroundFragment(poiInfoEntity));
    }

    private void handleFavoriteClick() {
        Logger.d(SEARCH_HMI_TAG, "点击收藏");
        if (poiInfoEntity != null) {
            SettingUpdateObservable.getInstance().onUpdateSyncTime();

            boolean isFavorite = !mScreenViewModel.isFavorite(poiInfoEntity).isEmpty();

            int favoriteIcon = isFavorite ? R.drawable.icon_basic_ic_star_default : R.drawable.img_basic_ic_star;
            mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(ContextCompat.getDrawable(getContext(), favoriteIcon));

            if (isFavorite) {
                mScreenViewModel.removeFavorite(poiInfoEntity);
                mScreenViewModel.deleteFavoriteData(poiInfoEntity.getFavoriteInfo().getItemId());
                ToastUtils.Companion.getInstance().showCustomToastView("取消收藏");
            } else {
                poiInfoEntity.getFavoriteInfo().setItemId(mScreenViewModel.addFavorite(poiInfoEntity));
                mScreenViewModel.addFavoriteData(poiInfoEntity, 0);
                ToastUtils.Companion.getInstance().showCustomToastView("收藏成功");
            }
        }
    }

    private void updateRouteButton() {
        boolean isAlongWaySearch = mScreenViewModel.isAlongWaySearch();
        Logger.d(SEARCH_HMI_TAG, isAlongWaySearch ? "添加途径点" : "去这里");

        int routeIcon = isAlongWaySearch ? R.drawable.img_basic_ic_add : R.drawable.icon_details_bottom_go_here;
        int routeText = isAlongWaySearch ? R.string.st_along_way_point : R.string.st_go_here;

        mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(ContextCompat.getDrawable(getContext(), routeIcon));
        mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(routeText);
    }


    private void intSearchLoadingDialog() {
        searchLoadingDialog = new SearchLoadingDialog(getContext());
    }

    public void onSearchResult(SearchResultEntity searchResultEntity) {
        if (null != searchLoadingDialog) {
            searchLoadingDialog.hide();
        }
        if (null == searchResultEntity || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            return;
        }
        Logger.d(SEARCH_HMI_TAG, "poiType: " + searchResultEntity.getPoiType());
        if (searchResultEntity.getPoiType() == 0) {
            CityDataInfo cityDataInfo = mScreenViewModel.getCityInfo(mScreenViewModel.getAcCode());
            if (cityDataInfo != null) {
                Logger.d(SEARCH_HMI_TAG, "城市数据信息: " + cityDataInfo.name +"，城市编码: " + mScreenViewModel.getAcCode());
                mViewBinding.poiOfflineHint.setVisibility(VISIBLE);
                mViewBinding.poiOfflineHint.setText(getContext().getString(R.string.search_offline_hint, cityDataInfo.name));
            }
        }
        this.poiInfoEntity = searchResultEntity.getPoiList().get(0);
        if (mViewBinding != null && poiInfoEntity != null) {
            mViewBinding.skPoiName.setText(poiInfoEntity.getName());
            mViewBinding.poiSecondAddress.setText(poiInfoEntity.getAddress());
            mViewBinding.scenePoiDetailsNormalView.poiBusinessHours.setText(getContext().getString(R.string.business_hour, poiInfoEntity.getBusinessTime()));
            if (ConvertUtils.isEmpty(poiInfoEntity.getPhone())) {
                mViewBinding.scenePoiDetailsNormalView.poiPhone.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPhone.setVisibility(View.GONE);
            }
            mViewBinding.scenePoiDetailsNormalView.poiPhone.setText(getContext().getString(R.string.poi_phone, poiInfoEntity.getPhone()));
            mViewBinding.scenePoiDetailsBottomView.stlPhone.setOnClickListener(v -> {
                String phone = poiInfoEntity.getPhone();
                List<String> phoneString = new ArrayList<>();
                if (phone.contains(";")) {
                    String[] split = phone.split(";");
                    phoneString.addAll(Arrays.asList(split));
                } else {
                    phoneString.add(phone);
                }
                if (!ConvertUtils.isEmpty(phoneString) && !ConvertUtils.isEmpty(phoneString.get(0))) {
                    Logger.d(SEARCH_HMI_TAG, "call phone: " + phoneString.get(0));
                    new SearchConfirmDialog.Build(getContext())
                            .setDialogObserver(new IBaseDialogClickListener() {
                                @Override
                                public void onCommitClick() {
                                    //拨打电话
                                    Intent intent = new Intent();
                                    intent.setAction(Intent.ACTION_CALL);
                                    intent.setData(Uri.parse("tel:" + phoneString.get(0)));
                                    Context context = getContext();
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
                    Logger.d(SEARCH_HMI_TAG, "call phone is null ");
                }

            });
            FavoriteInfo favoriteInfo = new FavoriteInfo()
                    .setCommonName(0)
                    .setUpdateTime(new Date().getTime());

            String itemId = mScreenViewModel.isFavorite(poiInfoEntity);
            poiInfoEntity.setFavoriteInfo(favoriteInfo);
            if (!itemId.isEmpty()) {
                poiInfoEntity.getFavoriteInfo().setItemId(itemId);
                mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(ContextCompat.getDrawable(getContext(), R.drawable.img_basic_ic_star));
            } else {
                mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(ContextCompat.getDrawable(getContext(), R.drawable.icon_basic_ic_star_default));
            }
            mScreenViewModel.getTravelTimeFuture(new GeoPoint(poiInfoEntity.getPoint().lon, poiInfoEntity.getPoint().lat))
                    .thenAccept(etaInfo -> {
                        ThreadManager.getInstance().postUi(new Runnable() {
                            @Override
                            public void run() {
                                String distance = formatDistanceArrayInternal(etaInfo.getDistance());
                                mViewBinding.poiDistanceTime.setText(MessageFormat.format("{0}  {1}", distance, etaInfo.getTravelTime()));
                            }
                        });

                    })
                    .exceptionally(error -> {
                        Logger.d(SEARCH_HMI_TAG, "getTravelTimeFuture error:" + error);
                        return null;
                    });
        } else {
            Logger.d(SEARCH_HMI_TAG, "mViewBinding or poiInfoEntity is null");
        }
        if (poiInfoEntity != null && mScreenViewModel != null) {
            int pointTypeCode = mScreenViewModel.getPointTypeCode(poiInfoEntity.getPointTypeCode());
            Logger.d(SEARCH_HMI_TAG, "pointTypeCode is: " + pointTypeCode);
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

    private void refreshGasStationView() {
        List<GasStationInfo> gasStationInfos = poiInfoEntity.getStationList();
        for (GasStationInfo gasStationInfo : gasStationInfos) {
            gasStationInfo.setPrice(getContext().getString(R.string.oil_price, gasStationInfo.getPrice()));
        }
        GasStationAdapter gasStationAdapter = new GasStationAdapter();
        gasStationAdapter.setGasStationList(gasStationInfos);
        mViewBinding.scenePoiDetailsGasStationView.poiGasBusinessHours.setText(getContext().getString(R.string.business_hour, poiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(poiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsGasStationView.poiGasPhone.setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsGasStationView.poiGasPhone.setText(getContext().getString(R.string.poi_phone, poiInfoEntity.getPhone()));
        mViewBinding.scenePoiDetailsGasStationView.poiGasOilList.setLayoutManager(new GridLayoutManager(getContext(), spanCount));
        mViewBinding.scenePoiDetailsGasStationView.poiGasOilList.addItemDecoration(new GridSpacingItemDecoration(getContext(), spanCount, spacing, horizontalSpacing, false));
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

    private void refreshChargeStationView() {
        Logger.d(SEARCH_HMI_TAG, "imageUrl is: " + poiInfoEntity.getImageUrl());
        List<ChargeInfo> chargeInfos = poiInfoEntity.getChargeInfoList();
        ChargeInfo chargeInfo = chargeInfos.get(0);
        if (chargeInfo.getSlowVolt() == 0 && chargeInfo.getSlowPower() == 0
                && chargeInfo.getSlow_free() == 0 && chargeInfo.getSlow_total() == 0) {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowLayout.setVisibility(GONE);
        } else {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowLayout.setVisibility(VISIBLE);
        }
        if (chargeInfo.getFastVolt() == 0 && chargeInfo.getFastPower() == 0
                && chargeInfo.getFast_free() == 0 && chargeInfo.getFast_total() == 0) {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastLayout.setVisibility(GONE);
        } else {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastLayout.setVisibility(VISIBLE);
        }
        String fastFree = chargeInfo.getFast_free() == 0 ? "--" : chargeInfo.getFast_free() + "";
        String fastTotal = chargeInfo.getFast_total() == 0 ? "--" : "/" + chargeInfo.getFast_total();
        String fastVolt = chargeInfo.getFastVolt() == 0 ? "--" : chargeInfo.getFastVolt() + "v";
        String fastPower = chargeInfo.getFastPower() == 0 ? "--" : chargeInfo.getFastPower() + "kw";
        String fastInfo = fastPower + "." + fastVolt;
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastOccupied.setText(fastFree);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastTotal.setText(fastTotal);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastCurrentAndVlot.setText(fastInfo);
        String slowFree = chargeInfo.getSlow_free() == 0 ? "--" : chargeInfo.getSlow_free() + "";
        String slowTotal = chargeInfo.getSlow_total() == 0 ? "--" : "/" + chargeInfo.getSlow_total();
        String slowVolt = chargeInfo.getSlowVolt() == 0 ? "--" : chargeInfo.getSlowVolt() + "v";
        String slowPower = chargeInfo.getSlowPower() == 0 ? "--" : chargeInfo.getSlowPower() + "kw";
        String slowInfo = slowPower + "." + slowVolt;
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowOccupied.setText(slowFree);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowTotal.setText(slowTotal);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot.setText(slowInfo);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargePrice.setText(getContext().getString
                (R.string.charge_price, chargeInfo.getCurrentElePrice()));
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice.setText(getContext().getString
                (R.string.charge_park_price, chargeInfo.getCurrentServicePrice()));
        mViewBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours.setText(getContext().getString(R.string.business_hour, poiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(poiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setText(getContext().getString(R.string.poi_phone, poiInfoEntity.getPhone()));
        String imageUrl = poiInfoEntity.getImageUrl();
        ViewAdapterKt.loadImageUrl(mViewBinding.scenePoiDetailsChargingStationView.poiChargeImg, imageUrl, R.drawable.test_pic, R.drawable.test_pic);
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

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

    private void refreshCateringView() {
        String rating = poiInfoEntity.getRating();
        if (TextUtils.isEmpty(rating)) {
            mViewBinding.scenePoiDetailsCateringView.poiCateringMarkValue.setVisibility(GONE);
            mViewBinding.scenePoiDetailsCateringView.poiCateringMarkLayout.setVisibility(GONE);
        } else {
            mViewBinding.scenePoiDetailsCateringView.poiCateringMarkValue.setText(rating);
            float realRating = Float.parseFloat(rating);
            if (realRating >= 0.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg1.setImageResource(R.drawable.img_collect_48);
            } else if (realRating < 0.5 && realRating > 0) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg1.setImageResource(R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg1.setImageResource(R.drawable.img_collect3_42);
            }
            if (realRating >= 1.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg2.setImageResource(R.drawable.img_collect_48);
            } else if (realRating < 1.5 && realRating > 1) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg2.setImageResource(R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg2.setImageResource(R.drawable.img_collect3_42);
            }
            if (realRating >= 2.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg3.setImageResource(R.drawable.img_collect_48);
            } else if (realRating < 2.5 && realRating > 2) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg3.setImageResource(R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg3.setImageResource(R.drawable.img_collect3_42);
            }
            if (realRating >= 3.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg4.setImageResource(R.drawable.img_collect_48);
            } else if (realRating < 3.5 && realRating > 3) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg4.setImageResource(R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg4.setImageResource(R.drawable.img_collect3_42);
            }
            if (realRating >= 4.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg5.setImageResource(R.drawable.img_collect_48);
            } else if (realRating < 4.5 && realRating > 4) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg5.setImageResource(R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg5.setImageResource(R.drawable.img_collect3_42);
            }
        }
        String avgCost;
        if (poiInfoEntity.getAverageCost() == -1) {
            avgCost = "--";
            mViewBinding.scenePoiDetailsCateringView.poiCateringPricePerPerson.setVisibility(GONE);
        } else {
            avgCost = getContext().getString(R.string.catering_price, poiInfoEntity.getAverageCost());
            mViewBinding.scenePoiDetailsCateringView.poiCateringPricePerPerson.setVisibility(VISIBLE);
        }
        mViewBinding.scenePoiDetailsCateringView.poiCateringHours.setText(getContext().getString(R.string.business_hour, poiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(poiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsCateringView.poiCateringPhone.setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsCateringView.poiCateringPhone.setText(getContext().getString(R.string.poi_phone, poiInfoEntity.getPhone()));
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

    private void refreshParkingLotView() {
        if (poiInfoEntity == null || poiInfoEntity.getParkingInfoList() == null || poiInfoEntity.getParkingInfoList().isEmpty()) {
            return;
        }
        ParkingInfo parkingInfo = poiInfoEntity.getParkingInfoList().get(0);
        String parkString = "";
        int spaceFree = parkingInfo.getSpaceFree();
        int spaceTotal = parkingInfo.getSpaceTotal();
        Logger.d(SEARCH_HMI_TAG, "spaceFree :" + spaceFree + " spaceTotal :" + spaceTotal);
        if (spaceFree == -1 && spaceTotal == -1) {
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setVisibility(GONE);
        } else if (spaceFree == -1) {
            parkString = getContext().getString(R.string.parking_lot_total, spaceTotal);
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setText(parkString);
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setTextColor(R.color.search_loading_bg_80);
        } else {
            parkString = getContext().getString(R.string.parking_lot_status, spaceFree, spaceTotal);
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setText(parkString);
        }
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setText(parkString);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotHours.setText(getContext().getString(R.string.business_hour, poiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(poiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotPhone.setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotPhone.setText(getContext().getString(R.string.poi_phone, poiInfoEntity.getPhone()));
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    private void refreshServiceAreaView() {
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility.setVisibility(GONE);
        List<GasStationInfo> gasStationInfos = poiInfoEntity.getStationList();
        if (gasStationInfos == null || gasStationInfos.isEmpty()) {
            mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaOil.setVisibility(GONE);
        } else {
            LinearLayoutManager layoutManager1 = new LinearLayoutManager(getContext());
            layoutManager1.setOrientation(LinearLayoutManager.HORIZONTAL);
            mViewBinding.scenePoiDetailsServiceAreaView.routePoidetailGasStation.setLayoutManager(layoutManager1);
            RoutePOIGasStationAdapter serviceGasAdapter = new RoutePOIGasStationAdapter();
            mViewBinding.scenePoiDetailsServiceAreaView.routePoidetailGasStation.setAdapter(serviceGasAdapter);
            List<String> gasString = new ArrayList<>();
            for (GasStationInfo gasStationInfo : gasStationInfos) {
                gasString.add(gasStationInfo.getType());
            }
            Logger.d(SEARCH_HMI_TAG, "gasString = 1" + gasString + " size: " + gasStationInfos.size());
            serviceGasAdapter.setRouteBeanList(gasString);
        }
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours.setText(getContext().getString(R.string.business_hour, poiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(poiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setText(getContext().getString(R.string.poi_phone, poiInfoEntity.getPhone()));
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    private void refreshScenicSpotView() {
        List<ChildInfo> childInfoList = poiInfoEntity.getChildInfoList();
        PoiDetailsScenicChildAdapter scenicChildAdapter = new PoiDetailsScenicChildAdapter();
        if (childInfoList != null && !childInfoList.isEmpty()) {
            scenicChildAdapter.setChildInfoList(childInfoList);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.setLayoutManager(new GridLayoutManager(getContext(), spanCount));
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.addItemDecoration(new GridSpacingItemDecoration(getContext(),spanCount, childSpaceing, childSpaceing, false));
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.setAdapter(scenicChildAdapter);
            if (childInfoList.size() > 2) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.setVisibility(VISIBLE);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.setVisibility(GONE);
            }
            scenicChildAdapter.setItemClickListener(new PoiDetailsScenicChildAdapter.OnItemClickListener() {
                @Override
                public void onItemClick(int index, boolean isSelectIndex) {
                    ChildInfo childInfo = childInfoList.get(index);
                    childSelectInfo = new PoiInfoEntity()
                            .setName(childInfo.getName())
                            .setAddress(childInfo.getAddress())
                            .setPid(childInfo.getPoiId())
                            .setPoint(childInfo.getLocation());
                }
            });
        } else {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.setVisibility(GONE);
        }
        String hourTime =  poiInfoEntity.getBusinessTime();
        Logger.d(SEARCH_HMI_TAG, "hourTime : " + hourTime);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.setOnClickListener(v -> {
            scenicChildAdapter.setCollapse(!scenicChildAdapter.isCollapse());
            scenicChildAdapter.notifyDataSetChanged();
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.setImageResource(scenicChildAdapter.isCollapse() ? R.drawable.img_under_the_48 : R.drawable.img_up_48);
        });

        String rating = poiInfoEntity.getRating();
        if (TextUtils.isEmpty(rating)) {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkLayout.setVisibility(GONE);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkValue.setVisibility(GONE);
        } else {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkValue.setText(rating);
            float realRating = Float.parseFloat(rating);
            if (realRating >= 0.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg1.setImageResource(R.drawable.img_collect_48);
            } else if (realRating < 0.5 && realRating > 0){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg1.setImageResource(R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg1.setImageResource(R.drawable.img_collect3_42);
            }
            if (realRating >= 1.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg2.setImageResource(R.drawable.img_collect_48);
            } else if (realRating < 1.5 && realRating > 1){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg2.setImageResource(R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg2.setImageResource(R.drawable.img_collect3_42);
            }
            if (realRating >= 2.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg3.setImageResource(R.drawable.img_collect_48);
            } else if (realRating < 2.5 && realRating > 2){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg3.setImageResource(R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg3.setImageResource(R.drawable.img_collect3_42);
            }
            if (realRating >= 3.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg4.setImageResource(R.drawable.img_collect_48);
            } else if (realRating < 3.5 && realRating > 3){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg4.setImageResource(R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg4.setImageResource(R.drawable.img_collect3_42);
            }
            if (realRating >= 4.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg5.setImageResource(R.drawable.img_collect_48);
            } else if (realRating < 4.5 && realRating > 4){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg5.setImageResource(R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg5.setImageResource(R.drawable.img_collect3_42);
            }
        }
        String avgCost;
        if (poiInfoEntity.getAverageCost() == -1) {
            avgCost = "--";
        } else {
            avgCost = getContext().getString(R.string.catering_price, poiInfoEntity.getAverageCost());
        }
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.setImageResource(scenicChildAdapter.isCollapse() ? R.drawable.img_under_the_48 : R.drawable.img_up_48);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotHoursContent.setText(getContext().getString(R.string.business_hour, poiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(poiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPhone.setVisibility(View.GONE);
        }
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPhone.setText(getContext().getString(R.string.poi_phone, poiInfoEntity.getPhone()));
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

    private void refreshNormalView() {
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(VISIBLE);
        if (poiInfoEntity == null) {
            return;
        }
        List<ChildInfo> childInfoList = poiInfoEntity.getChildInfoList();
        PoiDetailsScenicChildAdapter scenicChildAdapter = new PoiDetailsScenicChildAdapter();
        if (childInfoList != null && !childInfoList.isEmpty()) {
            scenicChildAdapter.setChildInfoList(childInfoList);
            mViewBinding.scenePoiDetailsNormalView.poiChildList.setLayoutManager(new GridLayoutManager(getContext(), spanCount));
            mViewBinding.scenePoiDetailsNormalView.poiChildList.addItemDecoration(new GridSpacingItemDecoration(getContext(),spanCount, childSpaceing, childSpaceing, false));
            mViewBinding.scenePoiDetailsNormalView.poiChildList.setAdapter(scenicChildAdapter);
            if (childInfoList.size() > 2) {
                mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setVisibility(VISIBLE);
            } else {
                mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setVisibility(GONE);
            }
            scenicChildAdapter.setItemClickListener(new PoiDetailsScenicChildAdapter.OnItemClickListener() {
                @Override
                public void onItemClick(int index, boolean isSelectIndex) {
                    ChildInfo childInfo = childInfoList.get(index);
                    childSelectInfo = new PoiInfoEntity()
                            .setName(childInfo.getName())
                            .setAddress(childInfo.getAddress())
                            .setPid(childInfo.getPoiId())
                            .setPoint(childInfo.getLocation());
                }
            });
        } else {
            mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setVisibility(GONE);
        }
        mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setOnClickListener(v -> {
            scenicChildAdapter.setCollapse(!scenicChildAdapter.isCollapse());
            scenicChildAdapter.notifyDataSetChanged();
            mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setImageResource(scenicChildAdapter.isCollapse() ? R.drawable.img_under_the_48 : R.drawable.img_up_48);
        });
        mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setImageResource(scenicChildAdapter.isCollapse() ? R.drawable.img_under_the_48 : R.drawable.img_up_48);

    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        childSelectInfo = null;
        Logger.d(SEARCH_HMI_TAG, "onDestroy");
    }

    public void doSearch(PoiInfoEntity poiInfo) {
        if (null != searchLoadingDialog) {
            searchLoadingDialog.show();
        }
        mScreenViewModel.doSearch(poiInfo);
        //todo 目前尚未提供CVP点击接口，暂时使用经纬度实现，后续提供接口后修改实现
        if (poiType == PoiType.POI_MAP_CLICK) {
            GeoPoint currentLocation = mScreenViewModel.getCurrentLocation();
            Logger.d(SEARCH_HMI_TAG, "当前位置 lon" + currentLocation.lon + " , lat" + currentLocation.lat
                    + " 目标位置 lon" + poiInfo.getPoint().lon + " , lat" + poiInfo.getPoint().lat);
            if (currentLocation.lon == poiInfo.getPoint().lon && currentLocation.lat == poiInfo.getPoint().lat) {
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
    public void refreshPoiView(int poiType) {
        if (mViewBinding == null) {
            return;
        }
        this.poiType = poiType;
        Logger.d(SEARCH_HMI_TAG, "poiType: " + poiType);
        refreshNormalView();
        //刷新View
        switch (poiType) {
            case PoiType.POI_SUGGESTION:
            case PoiType.POI_KEYWORD:
            case PoiType.POI_MAP_CLICK:
            case PoiType.POI_AROUND:
                if (mScreenViewModel.isAlongWaySearch()) {
                    Logger.d(SEARCH_HMI_TAG, "添加途径点");
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_along_way_point);
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(GONE);
                    mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                    mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                } else {
                    Logger.d(SEARCH_HMI_TAG, "去这里");
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(ContextCompat.getDrawable(getContext(), R.drawable.icon_details_bottom_go_here));
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_go_here);
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(VISIBLE);
                    mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(VISIBLE);
                    mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                }
                if (poiType == PoiType.POI_MAP_CLICK) {
                    //地图选点不需要展示电话和营业时间界面
                    mViewBinding.poiContentLayout.setVisibility(View.GONE);
                }
                break;
            case PoiType.POI_HOME:
                Logger.d(SEARCH_HMI_TAG, "设置为家");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_add_home);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                break;
            case PoiType.POI_COMPANY:
                Logger.d(SEARCH_HMI_TAG, "设置为公司");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_add_company);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                break;
            case PoiType.POI_COLLECTION:
                Logger.d(SEARCH_HMI_TAG, "添加");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(ContextCompat.getDrawable(getContext(), R.drawable.icon_details_bottom_go_here));
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_collect_add);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.VISIBLE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(GONE);
                break;
            case PoiType.POI_COMMON:
                Logger.d(SEARCH_HMI_TAG, "添加");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_collect_add);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                break;
            default:
                break;
        }
        //注册点击事件
        registerClickEvent(poiType);
    }

    private void registerClickEvent(int poiType) {
        mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "onClick poiType: " + poiType);
            switch (poiType) {
                case PoiType.POI_SUGGESTION:
                case PoiType.POI_KEYWORD:
                case PoiType.POI_AROUND:
                case PoiType.POI_MAP_CLICK:
                    if (childSelectInfo != null) {
                        if (SearchPackage.getInstance().isAlongWaySearch()) {
                            RoutePackage.getInstance().addViaPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP, childSelectInfo, RoutePoiType.ROUTE_POI_TYPE_WAY);
                        } else {
                            Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.ROUTE_FRAGMENT).navigation();
                            addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(childSelectInfo));
                        }
                    }else {
                        if (SearchPackage.getInstance().isAlongWaySearch()) {
                            RoutePackage.getInstance().addViaPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
                        } else {
                            Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.ROUTE_FRAGMENT).navigation();
                            addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                        }
                    }

                    break;
                case PoiType.POI_HOME:
                case PoiType.POI_COMPANY:
                case PoiType.POI_COLLECTION:
                case PoiType.POI_COMMON:
                    //  1，家  2，公司 3，常去地址  0，普通收藏点;
                    if (null != poiInfoEntity) {
                        int commonName;
                        String resultText = switch (poiType) {
                            case PoiType.POI_HOME -> {
                                commonName = 1;
                                closeAllFragmentsUntilTargetFragment("HomeCompanyFragment");
                                yield "设置家成功";
                            }
                            case PoiType.POI_COMPANY -> {
                                commonName = 2;
                                closeAllFragmentsUntilTargetFragment("HomeCompanyFragment");
                                yield "设置公司成功";
                            }
                            case PoiType.POI_COMMON -> {
                                commonName = 3;
                                closeAllFragmentsUntilTargetFragment("HomeCompanyFragment");
                                yield "添加成功";
                            }
                            default -> {
                                commonName = 0;
                                //todo 收藏页面未完成，待后续完善后添加跳转逻辑
                                yield "添加成功";
                            }
                        };
                        FavoriteInfo favoriteInfo = new FavoriteInfo();
                        favoriteInfo.setCommonName(commonName)
                                .setItemId(poiInfoEntity.getPid() + "_" + poiInfoEntity.getName() + "_" + poiInfoEntity.getPoint().getLon() + "_" + poiInfoEntity.getPoint().getLat())
                                .setUpdateTime(new Date().getTime());
                        poiInfoEntity.setFavoriteInfo(favoriteInfo);
                        BehaviorPackage.getInstance().addFavoriteData(poiInfoEntity, commonName);
                        SettingUpdateObservable.getInstance().onUpdateSyncTime();
                        closeAllFragment();
                        ToastUtils.Companion.getInstance().showCustomToastView(resultText);
                    }
                    break;
            }
        });
    }

    private String formatDistanceArrayInternal(int distance) {
        String[] distanceArray = ConvertUtils.formatDistanceArray(AppContext.mContext, distance);
        return distanceArray[0] + distanceArray[1];
    }
}
