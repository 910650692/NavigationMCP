
package com.fy.navi.scene.ui.poi;

import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.TextUtils;
import android.text.style.LeadingMarginSpan;
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
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
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
    private boolean mViaAddType = true;
    private PoiDetailsScenicChildAdapter mScenicChildAdapter;
    private SearchResultEntity mSearchResultEntity;
    private boolean mIsOpenFromNavi;
    private int mChildSelectIndex = -1;
    private boolean mIsCollectStatus = false;

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
        mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setOnClickListener(v ->{
                if(!ConvertUtils.isEmpty(mPoiInfoEntity.getOperatorId())){
                    if(!mScreenViewModel.isSGMLogin()){
                        mScreenViewModel.startSGMLogin();
                    }else{
                        handleNetFavoriteClick();
                    }
                }else{
                    handleFavoriteClick();
                }

            }
        );

        updateRouteButton();
    }

    /**
     * 去这里按钮的点击事件
     */
    private void handleRouteClick() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "点击去这里");
        if (mChildSelectInfo != null) {
            if (SearchPackage.getInstance().isAlongWaySearch() && !RoutePackage.getInstance().isMaxRouteParam(MapType.MAIN_SCREEN_MAIN_MAP)) {
                RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                        mChildSelectInfo);
            } else {
                openRouteFragment(mChildSelectInfo);
            }
        }else {
            if (SearchPackage.getInstance().isAlongWaySearch()) {
                if (mViaAddType) {
                    if(RoutePackage.getInstance().isMaxRouteParam(MapType.MAIN_SCREEN_MAIN_MAP)){
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "handleRouteClick isMaxRouteParam");
                        return;
                    }
                    RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                            mPoiInfoEntity);
                } else {
                    RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP,
                            mPoiInfoEntity, true);
                }

            } else {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "end point1: " + mPoiInfoEntity.getPoint().getLon()
                            + " ,lat" + mPoiInfoEntity.getPoint().getLat());
                openRouteFragment(mPoiInfoEntity);
            }
        }

    }

    /**
     * 打开路线界面
     * @param poiInfoEntity poi信息
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_GO)
    private void openRouteFragment(final PoiInfoEntity poiInfoEntity) {
        final Fragment fragment = (Fragment) ARouter.getInstance().build(
                RoutePath.Route.ROUTE_FRAGMENT).navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));

        //for burying point
        final JSONObject params = new JSONObject();
        try {
            params.put(BuryConstant.Key.ROUTE_POI_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
            params.put(BuryConstant.Key.POI_INFO_ENTRY, poiInfoEntity);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        final BuryProperty properties = new BuryProperty.Builder()
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
        final BuryProperty properties = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, mPoiInfoEntity.getName())
                .build();
        BuryPointController.getInstance().setBuryProps(properties);
    }

    @HookMethod
    private void sendBuryPointForAddFavorite(final String name, final int type) {
        String eventName = "";
        String key = BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION;
        switch (type){
            case 0:
                eventName = BuryConstant.EventName.AMAP_SETTING_FAVORITE_ADD;
                key = BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS;
                break;
            case 1:
                eventName = BuryConstant.EventName.AMAP_HOME_SAVE;
                break;
            case 2:
                eventName = BuryConstant.EventName.AMAP_WORK_SAVE;
                break;
            case 3:
                eventName = BuryConstant.EventName.AMAP_SETTING_HOT_ADD;
                key = BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS;
                break;
        }
        BuryPointController.getInstance().setEventName(eventName);
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(key, name)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_FAVORITE_SAVE)
    private void sendBuryPointForAddFavoriteFromMap(final String name) {
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, name)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /**
     * 收藏按钮的点击事件
     */
    private void handleFavoriteClick() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "点击收藏");
        if (mPoiInfoEntity != null) {
            SettingUpdateObservable.getInstance().onUpdateSyncTime();

            final boolean isFavorite = !mScreenViewModel.isFavorite(mPoiInfoEntity).isEmpty();

            final int favoriteIcon = isFavorite ? R.drawable.img_star58 :
                    R.drawable.img_star_filling58;
            mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(
                    ContextCompat.getDrawable(getContext(), favoriteIcon));
            if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CAR_CLICK) {
                final int favIcon = isFavorite ? R.drawable.img_star_white58 :
                        R.drawable.img_star_filling58;
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                        ContextCompat.getDrawable(getContext(), favIcon));
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(isFavorite ? R.string.sha_favorite : R.string.sha_has_favorite);
            }

            if (isFavorite) {
                mScreenViewModel.removeFavorite(mPoiInfoEntity);
//                mScreenViewModel.deleteFavoriteData(mPoiInfoEntity.getFavoriteInfo().getItemId());
                ToastUtils.Companion.getInstance().showCustomToastView("取消收藏");
            } else {
                if (ConvertUtils.isEmpty(mPoiInfoEntity.getPid())) {
                    //逆地理搜索出的点无poiId，需自己拼接
                    mPoiInfoEntity.setPid(mPoiInfoEntity.getPoint().getLon() + ""
                            + mPoiInfoEntity.getPoint().getLat());
                }
                mScreenViewModel.addFavorite(mPoiInfoEntity, 0);
                sendBuryPointForAddFavoriteFromMap(mPoiInfoEntity.getName());
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "current : "
                        + mPoiInfoEntity.getPoint().getLon() + " " + mPoiInfoEntity.getPoint().getLat()
                        + " ID: " + mPoiInfoEntity.getPid() + " ,name: " + mPoiInfoEntity.getName());
//                mScreenViewModel.addFavoriteData(mPoiInfoEntity, 0);
                ToastUtils.Companion.getInstance().showCustomToastView("收藏成功");
            }
        }
    }

    private void handleNetFavoriteClick(){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "点击自营站收藏");
        mScreenViewModel.updateCollectStatus((Activity) getContext(),mPoiInfoEntity);
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
     * @param taskId 请求Id
     * @param searchResultEntity 数据实体类
     */
    public void onSearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        if (null == searchResultEntity || searchResultEntity.getPoiList().isEmpty() || ConvertUtils.isEmpty(mScreenViewModel)) {
            //ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "taskId: " + taskId
                + " currentId: " + mScreenViewModel.getMTaskId());
        if (!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) {
            return;
        }
        if (null != mSearchLoadingDialog) {
            mSearchLoadingDialog.dismiss();
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiType: " + searchResultEntity.getPoiType());
        if (searchResultEntity.getPoiType() == 0) {
            final CityDataInfo cityDataInfo;
            if(!ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
                final PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
                cityDataInfo = mScreenViewModel.getCityInfo(poiInfoEntity.getAdCode());
            } else {
                cityDataInfo = mScreenViewModel.getCityInfo(mScreenViewModel.getAcCode());
            }
            if (cityDataInfo != null) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "城市数据信息: " + cityDataInfo.getName() +"，城市编码: "
                        + mScreenViewModel.getAcCode());
                mViewBinding.poiOfflineHint.setVisibility(VISIBLE);
                mViewBinding.poiOfflineHint.setText(
                        getContext().getString(R.string.search_offline_hint, cityDataInfo.getName()));
            }
            mViewBinding.poiDetailsScroll.setMaxHeight(316);
            mViewBinding.sivArrivalCapacity.setVisibility(View.GONE);
            mViewBinding.poiDistanceTime.setVisibility(View.GONE);
            mViewBinding.poiArrivalCapacity.setVisibility(View.GONE);

        }
        mSearchResultEntity = searchResultEntity;
        this.mPoiInfoEntity = searchResultEntity.getPoiList().get(0);
        initNormalView();
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
        if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CAR_CLICK && mViewBinding != null) {
            mViewBinding.poiDistanceTime.setVisibility(View.GONE);
            mViewBinding.poiArrivalCapacity.setVisibility(View.GONE);
            mViewBinding.sivArrivalCapacity.setVisibility(View.GONE);
            mViewBinding.poiContentLayout.setVisibility(View.GONE);
            mViewBinding.poiDetailsSubLine.setVisibility(View.GONE);
        }
        if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CLICK) {
            //点击地图弹出的poi详情页不需要展示子点扎标
            if (null != mScreenViewModel) {
                ThreadManager.getInstance().postDelay(() -> mScreenViewModel.clearTypeMark(LayerPointItemType.SEARCH_CHILD_POINT), 400);
            }
        }
    }

    public void onCollectUpdate(String code){
        Logger.d("huangli","code: "+code);
        if("0000".equals(code)){
            mIsCollectStatus = !mIsCollectStatus;
            final int favoriteIcon = !mIsCollectStatus ? R.drawable.img_star58 :
                    R.drawable.icon_basic_ic_star_fav;
            mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(
                    ContextCompat.getDrawable(getContext(), favoriteIcon));
            ToastUtils.Companion.getInstance().showCustomToastView(mIsCollectStatus ? getContext().getString(R.string.sha_has_favorite) : getContext().getString(R.string.sha_cancel_favorite));
        }
    }

    /**
     * 搜索图层子点点击事件
     * @param index 点击下标
     */
    public void onMarkChildClickCallBack(final int index) {
        if (mPoiInfoEntity == null || ConvertUtils.isEmpty(mPoiInfoEntity.getChildInfoList())) {
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onMarkChildClickCallBack is: " + index);
        final List<ChildInfo> mChildList = mPoiInfoEntity.getChildInfoList();
        if (index < mChildList.size() && index >= 0) {
            final ChildInfo childInfo = mChildList.get(index);
            mChildSelectInfo = new PoiInfoEntity()
                    .setName(childInfo.getName())
                    .setAddress(childInfo.getAddress())
                    .setPid(childInfo.getPoiId())
                    .setPoint(childInfo.getLocation());
            for (int i = 0; i < mChildList.size(); i++) {
                if (i == index) {
                    mChildList.get(i).setChecked(1);
                } else {
                    mChildList.get(i).setChecked(-1);
                }
            }
            if (mScenicChildAdapter != null) {
                mScenicChildAdapter.setChildInfoList(mChildList);
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
            String text = getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()).replace(";",";\n");
            SpannableString spannable = new SpannableString(text);
            int newLinePos = text.indexOf("\n");
            if (newLinePos >= 0) {
                spannable.setSpan(
                        new LeadingMarginSpan.Standard((int) ResourceUtils.Companion.getInstance().getDimension(com.fy.navi.ui.R.dimen.dp_90), 0),
                        newLinePos + 1,
                        text.length(),
                        Spannable.SPAN_EXCLUSIVE_EXCLUSIVE
                );
            }
            mViewBinding.scenePoiDetailsNormalView.poiPhone.setText(spannable);
            mViewBinding.scenePoiDetailsBottomView.stlPhone.setOnClickListener(new OnClickListener() {
                @Override
                @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_PHONE)
                public void onClick(final View v) {
                    final String phone = mPoiInfoEntity.getPhone();
                    final List<String> phoneString = new ArrayList<>();
                    final StringBuffer phoneProp = new StringBuffer();
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
                                        try {
                                            //拨打电话
                                            final Intent intent = new Intent();
                                            intent.setAction(Intent.ACTION_CALL);
                                            intent.setData(Uri.parse("tel:" + phoneString.get(0)));
                                            final Context context = getContext();
                                            context.startActivity(intent);
                                        } catch (ActivityNotFoundException e) {// 提示用户无法拨打电话
                                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No app found to handle the call action: " + e.getMessage());
                                        } catch (SecurityException e) {// 提示用户权限不足
                                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Permission denied for making a call: " + e.getMessage());
                                        } catch (NullPointerException e) {// 提示用户数据无效
                                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Null pointer exception: " + e.getMessage());
                                        } catch (IllegalArgumentException e) {// 提示用户电话号码无效
                                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Invalid phone number: " + e.getMessage());
                                        }
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
                    final BuryProperty buryProperty = new BuryProperty.Builder()
                            .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, phoneProp.toString())
                            .build();
                    BuryPointController.getInstance().setBuryProps(buryProperty);

                }
            });
            final FavoriteInfo favoriteInfo = new FavoriteInfo()
                    .setCommonName(0)
                    .setUpdateTime(new Date().getTime());
            if (ConvertUtils.isEmpty(mPoiInfoEntity.getPid())) {
                //逆地理搜索出的点无poiId，需自己拼接
                mPoiInfoEntity.setPid(mPoiInfoEntity.getPoint().getLon() + ""
                        + mPoiInfoEntity.getPoint().getLat());
            }
            final String itemId = mScreenViewModel.isFavorite(mPoiInfoEntity);
            mPoiInfoEntity.setFavoriteInfo(favoriteInfo);
            if (!itemId.isEmpty() || mPoiInfoEntity.getIsCollect()) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"IsCollect");
                mIsCollectStatus = true;
                mPoiInfoEntity.getFavoriteInfo().setItemId(itemId);
                mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(
                        ContextCompat.getDrawable(getContext(), R.drawable.icon_basic_ic_star_fav));
                if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CAR_CLICK) {
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                            ContextCompat.getDrawable(getContext(), R.drawable.icon_basic_ic_star_fav));
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.sha_has_favorite);
                }
            } else {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"IsNoCollect");
                mIsCollectStatus = false;
                mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(
                        ContextCompat.getDrawable(getContext(),
                                R.drawable.img_star58));
                if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CAR_CLICK) {
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                            ContextCompat.getDrawable(getContext(), R.drawable.img_star_white58));
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.sha_favorite);
                }
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
        String text = getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()).replace(";",";\n");
        SpannableString spannable = new SpannableString(text);
        int newLinePos = text.indexOf("\n");
        if (newLinePos >= 0) {
            spannable.setSpan(
                    new LeadingMarginSpan.Standard((int) ResourceUtils.Companion.getInstance().getDimension(com.fy.navi.ui.R.dimen.dp_90), 0),
                    newLinePos + 1,
                    text.length(),
                    Spannable.SPAN_EXCLUSIVE_EXCLUSIVE
            );
        }
        mViewBinding.scenePoiDetailsGasStationView.poiGasPhone.setText(spannable);
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
        if (!ConvertUtils.isEmpty(chargeInfos)) {
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
                            R.string.charge_price, ConvertUtils.stringFormatTwo(chargeInfo.getCurrentElePrice())));
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice.setText(
                    getContext().getString(
                            R.string.charge_park_price, chargeInfo.getCurrentServicePrice()));
            if(mSearchResultEntity.getIsNetData()){
                mViewBinding.scenePoiDetailsChargingStationView.poiChargePriceAllday.setVisibility(VISIBLE);
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeAppointment.setVisibility(VISIBLE);
            }else{
                mViewBinding.scenePoiDetailsChargingStationView.poiChargePriceAllday.setVisibility(GONE);
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeAppointment.setVisibility(GONE);
            }
        }
        mViewBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours.setText(
                getContext().getString(R.string.business_hour, mPoiInfoEntity.getBusinessTime()));
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.
                    setVisibility(View.GONE);
        }
        String text = getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()).replace(";",";\n");
        SpannableString spannable = new SpannableString(text);
        int newLinePos = text.indexOf("\n");
        if (newLinePos >= 0) {
            spannable.setSpan(
                    new LeadingMarginSpan.Standard((int) ResourceUtils.Companion.getInstance().getDimension(com.fy.navi.ui.R.dimen.dp_80), 0),
                    newLinePos + 1,
                    text.length(),
                    Spannable.SPAN_EXCLUSIVE_EXCLUSIVE
            );
        }
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setText(spannable);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargePriceAllday.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View view) {
                final Fragment fragment = (Fragment) ARouter.getInstance().build(
                        RoutePath.Search.POI_CHARGE_PRICE_ALL_DAY_FRAGMENT).navigation();
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createChargePriceFragment(mPoiInfoEntity));
            }
        });
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastLayout.setOnClickListener(v ->{
            toReservationListView(AutoMapConstant.EquipmentType.FAST);
        });
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowLayout.setOnClickListener(v ->{
            toReservationListView(AutoMapConstant.EquipmentType.SLOW);
        });
        if(!ConvertUtils.isNull(mPoiInfoEntity.getReservationInfo())){
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeAppointment.setText(R.string.charge_appointment_look);
        }else{
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeAppointment.setText(R.string.charge_appointment);
        }
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeAppointment.setOnClickListener(v -> {
            if(!ConvertUtils.isNull(mPoiInfoEntity.getReservationInfo())){
                toReservationDetailView();
            }else{
                toReservationListView(-1);
            }
        });
        final String imageUrl = mPoiInfoEntity.getImageUrl();
        if(ConvertUtils.isNull(imageUrl)){
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeImgLayout.setVisibility(GONE);
        }else{
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeImgLayout.setVisibility(VISIBLE);
            ViewAdapterKt.loadImageUrl(mViewBinding.scenePoiDetailsChargingStationView.poiChargeImg,
                    imageUrl, R.drawable.test_pic, R.drawable.test_pic);
        }
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
        String text = getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()).replace(";",";\n");
        SpannableString spannable = new SpannableString(text);
        int newLinePos = text.indexOf("\n");
        if (newLinePos >= 0) {
            spannable.setSpan(
                    new LeadingMarginSpan.Standard((int) ResourceUtils.Companion.getInstance().getDimension(com.fy.navi.ui.R.dimen.dp_90), 0),
                    newLinePos + 1,
                    text.length(),
                    Spannable.SPAN_EXCLUSIVE_EXCLUSIVE
            );
        }
        mViewBinding.scenePoiDetailsCateringView.poiCateringPhone.setText(spannable);
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
        String text = getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()).replace(";",";\n");
        SpannableString spannable = new SpannableString(text);
        int newLinePos = text.indexOf("\n");
        if (newLinePos >= 0) {
            spannable.setSpan(
                    new LeadingMarginSpan.Standard((int) ResourceUtils.Companion.getInstance().getDimension(com.fy.navi.ui.R.dimen.dp_90), 0),
                    newLinePos + 1,
                    text.length(),
                    Spannable.SPAN_EXCLUSIVE_EXCLUSIVE
            );
        }
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotPhone.setText(spannable);
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
        String text = getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()).replace(";",";\n");
        SpannableString spannable = new SpannableString(text);
        int newLinePos = text.indexOf("\n");
        if (newLinePos >= 0) {
            spannable.setSpan(
                    new LeadingMarginSpan.Standard((int) ResourceUtils.Companion.getInstance().getDimension(com.fy.navi.ui.R.dimen.dp_90), 0),
                    newLinePos + 1,
                    text.length(),
                    Spannable.SPAN_EXCLUSIVE_EXCLUSIVE
            );
        }
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setText(spannable);
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
        mScenicChildAdapter = new PoiDetailsScenicChildAdapter();
        if (childInfoList != null && !childInfoList.isEmpty()) {
            if (mChildSelectIndex != -1 && mChildSelectIndex < childInfoList.size()) {
                childInfoList.get(mChildSelectIndex).setChecked(1);
                final ChildInfo childInfo = childInfoList.get(mChildSelectIndex);
                mChildSelectInfo = new PoiInfoEntity()
                        .setName(childInfo.getName())
                        .setAddress(childInfo.getAddress())
                        .setPid(childInfo.getPoiId())
                        .setPoint(childInfo.getLocation());
            }
            mScenicChildAdapter.setChildInfoList(childInfoList);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.setVisibility(View.VISIBLE);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.setLayoutManager(
                    new GridLayoutManager(getContext(), mSpanCount));
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.addItemDecoration(
                    new GridSpacingItemDecoration(getContext(), mSpanCount, mChildSpacing,
                            mChildSpacing, false));
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.
                    setAdapter(mScenicChildAdapter);
            if (childInfoList.size() > 2) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                        setVisibility(VISIBLE);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                        setVisibility(GONE);
            }
            mScenicChildAdapter.setItemClickListener((index, isSelectIndex) -> {
                if (isSelectIndex) {
                    final ChildInfo childInfo = childInfoList.get(index);
                    mChildSelectInfo = new PoiInfoEntity()
                            .setName(childInfo.getName())
                            .setAddress(childInfo.getAddress())
                            .setPid(childInfo.getPoiId())
                            .setPoint(childInfo.getLocation());
                    mScreenViewModel.setChildIndex(index);
                } else {
                    mChildSelectInfo = null;
                }
            });
        } else {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.setVisibility(View.GONE);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                    setVisibility(GONE);
        }
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                setOnClickListener(v -> {
                    mScenicChildAdapter.setCollapse(!mScenicChildAdapter.isCollapse());
                    mScenicChildAdapter.notifyDataSetChanged();
                    mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                            setImageResource(mScenicChildAdapter.isCollapse() ?
                                    R.drawable.img_under_the_48 : R.drawable.img_up_48);
                });
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                setImageResource(mScenicChildAdapter.isCollapse() ? R.drawable.img_under_the_48 :
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
        if (mPoiInfoEntity.getAverageCost() == -1 || mPoiInfoEntity.getAverageCost() == 0) {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPrice.setText("--");
        } else {
            avgCost = getContext().getString(R.string.catering_price, mPoiInfoEntity.getAverageCost());
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPrice.setText(avgCost);
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getBusinessTime())) {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotHoursContent.setVisibility(View.GONE);
        } else {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotHoursContent.setText(
                    getContext().getString(R.string.business_hour, mPoiInfoEntity.getBusinessTime()));
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPhone.setVisibility(View.GONE);
        }
        String text = getContext().getString(R.string.poi_phone, mPoiInfoEntity.getPhone()).replace(";",";\n");
        SpannableString spannable = new SpannableString(text);
        int newLinePos = text.indexOf("\n");
        if (newLinePos >= 0) {
            spannable.setSpan(
                    new LeadingMarginSpan.Standard((int) ResourceUtils.Companion.getInstance().getDimension(com.fy.navi.ui.R.dimen.dp_90), 0),
                    newLinePos + 1,
                    text.length(),
                    Spannable.SPAN_EXCLUSIVE_EXCLUSIVE
            );
        }
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPhone.setText(spannable);
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
            if (mViewBinding.scenePoiDetailsNormalView.poiChildList.getItemDecorationCount() == 0) {
                mViewBinding.scenePoiDetailsNormalView.poiChildList.addItemDecoration(
                        new GridSpacingItemDecoration(getContext(), mSpanCount, mChildSpacing,
                                mChildSpacing, false));
            }
            mViewBinding.scenePoiDetailsNormalView.poiChildList.setAdapter(scenicChildAdapter);
            if (childInfoList.size() > 2) {
                mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.
                        setVisibility(VISIBLE);
            } else {
                mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setVisibility(GONE);
            }
            scenicChildAdapter.setItemClickListener((index, isSelectIndex) -> {
                if (isSelectIndex) {
                    final ChildInfo childInfo = childInfoList.get(index);
                    mChildSelectInfo = new PoiInfoEntity()
                            .setName(childInfo.getName())
                            .setAddress(childInfo.getAddress())
                            .setPid(childInfo.getPoiId())
                            .setPoint(childInfo.getLocation());
                    mScreenViewModel.setChildIndex(index);
                } else {
                    mChildSelectInfo = null;
                }
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
                mSearchLoadingDialog.dismiss();
                if (!ConvertUtils.isEmpty(mViewBinding)) {
                    mViewBinding.csPoiNoResult.setVisibility(View.VISIBLE);
                    mViewBinding.skPoiName.setVisibility(View.GONE);
                    mViewBinding.poiDetailsScroll.setVisibility(View.GONE);
                    mViewBinding.poiTypeIcon.setVisibility(View.GONE);
                    mViewBinding.scenePoiDetailsBottomView.getRoot().setVisibility(View.GONE);

                    mViewBinding.noResultButton.setOnClickListener((view) -> {
                        doSearch(mPoiInfoEntity);
                        mViewBinding.csPoiNoResult.setVisibility(View.GONE);
                        mViewBinding.skPoiName.setVisibility(View.VISIBLE);
                        mViewBinding.poiDetailsScroll.setVisibility(View.VISIBLE);
                        mViewBinding.poiTypeIcon.setVisibility(View.VISIBLE);
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
        if (null != mSearchLoadingDialog && mSearchLoadingDialog.isShowing()) {
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "mSearchLoadingDialog is showing");
        } else {
            mSearchLoadingDialog = new SearchLoadingDialog(getContext());
            mSearchLoadingDialog.show();
        }
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        ThreadManager.getInstance().postDelay(mTimeoutTask, 6000);
        mPoiInfoEntity = poiInfo;
        if(ConvertUtils.isEmpty(poiInfo.getOperatorId())){
            mScreenViewModel.doSearch(poiInfo);
        }else{
            mScreenViewModel.doSearchByNet(poiInfo);
        }
        if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CAR_CLICK && mViewBinding != null) {
            mViewBinding.poiDistanceTime.setVisibility(View.GONE);
            mViewBinding.poiArrivalCapacity.setVisibility(View.GONE);
            mViewBinding.sivArrivalCapacity.setVisibility(View.GONE);
            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setVisibility(View.VISIBLE);
            mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setVisibility(View.GONE);
            mViewBinding.scenePoiDetailsBottomView.stlFunction.setVisibility(View.GONE);
        }
    }

    public void setChildIndex(final int index) {
        mChildSelectIndex = index;
    }

    /**
     * 退回到详情页面时，重新按照搜索结果列表进行扎标
     * @param poiInfoEntities 需要重新扎标的列表
     */
    public void reloadLastPoiMarker(final List<PoiInfoEntity> poiInfoEntities) {
        mScreenViewModel.addPoiMarker(poiInfoEntities, 0);
    }

    /**
     * 退回到详情页面时，重新按照POI详情页数据进行扎标
     */
    public void reloadPoiLabelMarker() {
        mScreenViewModel.createLabelMarker(mSearchResultEntity);
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
     *                POI_MAP_CAR_CLICK = 9;//自车位点击事件
     * @param poiInfoEntity poi信息
     */
    public void refreshPoiView(final int poiType, final PoiInfoEntity poiInfoEntity) {
        if (mViewBinding == null) {
            return;
        }
        this.mPoiType = poiType;
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiType: " + poiType);
        final int pointTypeCode = mScreenViewModel.getPointTypeCode(poiInfoEntity.getPointTypeCode());
        Logger.d("huangli","pointTypeCode: "+poiInfoEntity.getOperatorId());
        if(ConvertUtils.isEmpty(poiInfoEntity.getOperatorId())){
            refreshNormalView();
        }
        //刷新View
        switch (poiType) {
            case AutoMapConstant.PoiType.POI_SUGGESTION:
            case AutoMapConstant.PoiType.POI_KEYWORD:
            case AutoMapConstant.PoiType.POI_MAP_CLICK:
            case AutoMapConstant.PoiType.POI_AROUND:
                if (mScreenViewModel.isAlongWaySearch()) {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "添加途径点");
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                    if (RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
                        mViaAddType = false;
                        if (RoutePackage.getInstance().isStartOrEndRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
                            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setAlpha(0.5f);
                            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setClickable(false);
                        } else {
                            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setAlpha(1.0f);
                            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setClickable(true);
                        }
                        mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_along_way_point_delete);
                    } else {
                        mViaAddType = true;
                        mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_along_way_point_add);
                        // 达到最大途径点禁止点击并置灰
                        if(RoutePackage.getInstance().isMaxRouteParam(MapType.MAIN_SCREEN_MAIN_MAP)){
                            mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setAlpha(0.5f);
                            mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setAlpha(0.5f);
                        }else{
                            mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setAlpha(1);
                            mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setAlpha(1);
                        }
                    }

                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(GONE);
                    mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                    if (mScreenViewModel.getViaCount() >= 1 && mViaAddType) {
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
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setAlpha(1.0f);
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_go_here);
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(VISIBLE);
                    mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(VISIBLE);
                    mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                }
                if (poiType == AutoMapConstant.PoiType.POI_MAP_CLICK) {
                    //地图选点不需要展示电话和营业时间界面
                    mViewBinding.poiContentLayout.setVisibility(View.GONE);
                    mViewBinding.poiDetailsSubLine.setVisibility(View.GONE);
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
                        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPid())) {
                            //逆地理搜索出的点无poiId，需自己拼接
                            mPoiInfoEntity.setPid(mPoiInfoEntity.getPoint().getLon() + ""
                                    + mPoiInfoEntity.getPoint().getLat());
                        }
                        mPoiInfoEntity.setFavoriteInfo(favoriteInfo);
                        mScreenViewModel.addFavorite(mPoiInfoEntity, commonName);
                        sendBuryPointForAddFavorite(mPoiInfoEntity.getName(), commonName);
                        //收藏逻辑执行完成后，会回到来源页，所以需要取消扎标
                        mScreenViewModel.clearLabelMarker();
//                        BehaviorPackage.getInstance().addFavoriteData(mPoiInfoEntity, commonName);
                        SettingUpdateObservable.getInstance().onUpdateSyncTime();
                        ToastUtils.Companion.getInstance().showCustomToastView(resultText);
                    }
                    break;
                case AutoMapConstant.PoiType.POI_DELETE_AROUND:
                    RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP,
                            mPoiInfoEntity, true);
                    break;
                case AutoMapConstant.PoiType.POI_MAP_CAR_CLICK:
                    handleFavoriteClick();
                    break;
                default:
                    break;
            }
        });
        mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setOnClickListener((view) -> {
            if (mChildSelectInfo != null) {
                if (SearchPackage.getInstance().isAlongWaySearch() && !RoutePackage.getInstance().isMaxRouteParam(MapType.MAIN_SCREEN_MAIN_MAP)) {
                    RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                            mChildSelectInfo, 0);
                }
            } else {
                if (SearchPackage.getInstance().isAlongWaySearch() && !RoutePackage.getInstance().isMaxRouteParam(MapType.MAIN_SCREEN_MAIN_MAP)) {
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

    public void toReservationListView(int type){
        if(!mSearchResultEntity.getIsNetData()){
            return;
        }
        if(!mScreenViewModel.isSGMLogin()){
            mScreenViewModel.startSGMLogin();
            return;
        }
        final Fragment fragment = (Fragment) ARouter.getInstance().build(
                RoutePath.Search.POI_CHARGE_RESERVATION_LIST_FRAGMENT).navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createEquipmentListFragment(type,mPoiInfoEntity));
    }

    public void toReservationDetailView(){
        final Fragment fragment = (Fragment) ARouter.getInstance().build(
                RoutePath.Search.POI_CHARGE_RESERVATION_DETAILS_FRAGMENT).navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createChargePriceFragment(mPoiInfoEntity));
    }

    public void setPowerType(final int powerType){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"powerType: "+powerType);
        mScreenViewModel.mPowerType.setValue(powerType);
    }

    /**
     * @param b 是否从导航进入的搜索页面
     */
    public void setNaviControl(boolean b) {
        mIsOpenFromNavi = b;
    }

    public boolean getIsOpenFromNavi() {
        return mIsOpenFromNavi;
    }
}
