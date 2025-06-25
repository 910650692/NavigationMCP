package com.sgm.navi.scene.ui.search;


import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.databinding.MainAlongWaySearchHistoryViewBinding;
import com.sgm.navi.scene.impl.search.SceneMainAlongWaySearchHistoryImpl;
import com.sgm.navi.scene.impl.search.SearchFragmentFactory;
import com.sgm.navi.scene.ui.adapter.SearchHistoryAdapter;
import com.sgm.navi.scene.ui.adapter.SearchResultAdapter;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RoutePoiType;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.greendao.history.History;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 沿途主页面历史记录 scene
 * @CreateDate: $ $
 */
public class SceneSearchHistoryView extends BaseSceneView<MainAlongWaySearchHistoryViewBinding, SceneMainAlongWaySearchHistoryImpl> {
    private SearchHistoryAdapter mSearchHistoryAdapter;
    private SearchResultAdapter mSearchResultAdapter;
    private SearchLoadingDialog mSearchLoadingDialog;
    private int mHomeCompanyType = -1;
    private static final String DIVIDER = "_";
    private static final String NODATA = "暂无数据";
    public SceneSearchHistoryView(@NonNull final Context context) {
        super(context);
    }

    public SceneSearchHistoryView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);

    }

    public SceneSearchHistoryView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected MainAlongWaySearchHistoryViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return MainAlongWaySearchHistoryViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneMainAlongWaySearchHistoryImpl initSceneImpl() {
        return new SceneMainAlongWaySearchHistoryImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setSceneMainAlongWaySearchHistoryImpl(mScreenViewModel);
    }

    /**
     * 设置家和公司类型
     * @param homeCompanyState 家和公司类型
     */
    public void setHomeCompanyState(final int homeCompanyState) {
        mHomeCompanyType = homeCompanyState;
        if (mSearchResultAdapter != null) {
            mSearchResultAdapter.setHomeCompanyType(homeCompanyState);
        }
        if (mSearchHistoryAdapter != null) {
            mSearchHistoryAdapter.setHomeCompanyType(homeCompanyState);
        }
    }

    /**
     * 初始化通用视图
     */
    private void initNormalView() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "SceneSearchHistoryView initObserver");
        mSearchLoadingDialog = new SearchLoadingDialog(getContext());
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.rcyRecordAlong.setLayoutManager(layoutManager);
        final LinearLayoutManager layoutManagerSuggestion = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.suggestResultListAlong.setLayoutManager(layoutManagerSuggestion);

        mSearchResultAdapter = new SearchResultAdapter();
        mViewBinding.suggestResultListAlong.setAdapter(mSearchResultAdapter);
        mSearchHistoryAdapter = new SearchHistoryAdapter();
        mSearchHistoryAdapter.setMIsOnlyShowNaviRecord(true);
        mViewBinding.rcyRecordAlong.setAdapter(mSearchHistoryAdapter);

        getSearchKeywordRecord();

        mViewBinding.sclDeleteRecordAlong.setOnClickListener(view -> {
            if (mSearchHistoryAdapter.getItemCount() == 0) {
                ToastUtils.Companion.getInstance().showCustomToastView(NODATA);
                return;
            }
            new SearchConfirmDialog.Build(getContext())
                    .setDialogObserver(new IBaseDialogClickListener() {
                        @Override
                        public void onCommitClick() {
                            //清空历史记录
                            UserTrackPackage.getInstance().clearSearchHistory();
                            UserTrackPackage.getInstance().clearHistoryRoute();
                            mSearchHistoryAdapter.notifyList(new ArrayList<>());
                            ToastUtils.Companion.getInstance().showCustomToastView("已清空历史记录");
                            mViewBinding.sclDeleteRecordAlong.setVisibility(GONE);
                            mViewBinding.rcyRecordAlong.setVisibility(GONE);
                            mViewBinding.tvRecordNullAlong.setVisibility(VISIBLE);
                        }

                        @Override
                        public void onCancelClick() {
                        }
                    })
                    .setTitle("提示")
                    .setContent("确定清空历史记录？")
                    .setConfirmTitle(ResourceUtils.Companion.getInstance().getString(R.string.dsc_confirm))
                    .build().show();
        });
    }

    @Override
    protected void initObserver() {
        initNormalView();
        mSearchHistoryAdapter.setOnItemClickListener(new SearchHistoryAdapter.ItemClickListener() {
            @Override
            public void onItemClick(final int position, final History history) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "BP====onItemClick position:" , position
                        , " history:" , history , " homeCompanyType:" , mHomeCompanyType);
                if (history.getMType() == AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY) {
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                            .navigation();
                    final String sourceFragment = getSourceFragment();
                    addFragment((BaseFragment) fragment,
                            SearchFragmentFactory.createKeywordFragment(
                                    sourceFragment, AutoMapConstant.SearchType.SEARCH_KEYWORD, history.getMKeyWord(), null));
                } else {
                    final GeoPoint historyPoint = parseGeoPoint(history.getMEndPoint());
                    final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                            .setPid(history.getMPoiId())
                            .setPoint(historyPoint);
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                            .navigation();
                    final int poiType = getType();
                    addFragment((BaseFragment) fragment,
                            SearchFragmentFactory.createPoiDetailsFragment(
                                    AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, poiType, poiInfoEntity));
                }
            }
            @Override
            public void onNaviClick(final int position, final History history) {
                final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                poiInfoEntity.setName(history.getMEndPoiName());
                poiInfoEntity.setAddress(history.getMEndPoiName());
                poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
                poiInfoEntity.setPid(history.getMPoiId());
                final int commonName = mSearchResultAdapter.getHomeCompanyType();
                final GeoPoint historyPoint = parseGeoPoint(history.getMEndPoint());
                final GeoPoint geoPoint = new GeoPoint();
                geoPoint.setLon(historyPoint.getLon());
                geoPoint.setLat(historyPoint.getLat());
                poiInfoEntity.setPoint(geoPoint);
                final FavoriteInfo favoriteInfo = new FavoriteInfo();
                favoriteInfo.setCommonName(commonName)
                        .setUpdateTime(new Date().getTime());
                poiInfoEntity.setFavoriteInfo(favoriteInfo);
                if (mHomeCompanyType == 1
                        || mHomeCompanyType == 2
                        || mHomeCompanyType == 3
                        || mHomeCompanyType == 0) {
                    if (mHomeCompanyType == 0) {
                        if (!mScreenViewModel.isFavorite(poiInfoEntity).isEmpty()) {
                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "current poi isFav,just return");
                            return;
                        }
                    }
                    if (mHomeCompanyType == 3) {
                        if (mScreenViewModel.isFrequentAddress(poiInfoEntity)) {
                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "current poi isFre,just return");
                            return;
                        }
                    }
                    String toastTip = switch (mHomeCompanyType) {
                        case 1 -> getResources().getString(R.string.mps_set_home_success);
                        case 2 -> getResources().getString(R.string.mps_set_company_success);
                        case 3 -> getResources().getString(R.string.smp_set_add);
                        default -> getResources().getString(R.string.mps_set_collect);
                    };
                    BehaviorPackage.getInstance().addFavorite(poiInfoEntity, commonName);
                    ToastUtils.Companion.getInstance().showCustomToastView(toastTip);
                    SettingUpdateObservable.getInstance().onUpdateSyncTime();
                    closeAllFragmentsUntilTargetFragment("HomeCompanyFragment");
                    showCurrentFragment();
                    sendBuryPointForAddFavorite(poiInfoEntity, commonName);
                } else {
                    if (SearchPackage.getInstance().isAlongWaySearch()) {
                        if (RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
                            if (RoutePackage.getInstance().isStartOrEndRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
                                ToastUtils.Companion.getInstance().showCustomToastView("起点终点不能删除");
                            } else {
                                RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, true);
                            }
                        } else {
                            RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
                        }

                    } else {
                        SearchPackage.getInstance().clearLabelMark();
                        final Fragment fragment = (Fragment) ARouter.getInstance()
                                .build(RoutePath.Route.ROUTE_FRAGMENT)
                                .navigation();
                        addFragment((BaseFragment) fragment,
                                SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                    }
                }

            }
        });
        mSearchResultAdapter.setOnItemClickListener(new SearchResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final int position, final PoiInfoEntity poiInfoEntity) {
                final Fragment fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                        .navigation();
                final int poiType = getType();
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                        AutoMapConstant.SourceFragment.SUG_SEARCH_FRAGMENT,
                                poiType, poiInfoEntity));
            }

            @Override
            public void onNaviClick(final int position, final PoiInfoEntity poiInfoEntity) {
//                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.smp_set_success), 0);
                final int commonName = mSearchResultAdapter.getHomeCompanyType();
                final FavoriteInfo favoriteInfo = new FavoriteInfo();
                favoriteInfo.setCommonName(commonName)
                        .setUpdateTime(new Date().getTime());
                poiInfoEntity.setFavoriteInfo(favoriteInfo);
                BehaviorPackage.getInstance().addFavorite(poiInfoEntity, commonName);
                sendBuryPointForAddFavorite(poiInfoEntity, commonName);
//                BehaviorPackage.getInstance().addFavoriteData(poiInfoEntity, commonName);
                SettingUpdateObservable.getInstance().onUpdateSyncTime();
                closeAllFragmentsUntilTargetFragment("HomeCompanyFragment");
                showCurrentFragment();
            }
        });
    }

    /**
     * 获取poi类型
     * @return poiType
     */
    private int getType() {
        int poiType = AutoMapConstant.PoiType.POI_KEYWORD;
        // 1:家 2:公司 3:常用地址 0:收藏夹 -1:都不是
        if (mHomeCompanyType == 1) {
            poiType = AutoMapConstant.PoiType.POI_HOME;
        } else if (mHomeCompanyType == 2) {
            poiType = AutoMapConstant.PoiType.POI_COMPANY;
        } else if (mHomeCompanyType == 3) {
            poiType = AutoMapConstant.PoiType.POI_COMMON;
        } else if (mHomeCompanyType == 0) {
            poiType = AutoMapConstant.PoiType.POI_COLLECTION;
        }
        return poiType;
    }

    /**
     * 获取来源fragment
     * @return sourceFragment
     */
    @NonNull
    private String getSourceFragment() {
        final String sourceFragment;
        if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.HOME) {
            sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_HOME;
        } else if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.COMPANY) {
            sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_COMPANY;
        } else if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.COMMON) {
            sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_COMMON;
        } else if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.COLLECTION) {
            sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION;
        } else {
            sourceFragment = AutoMapConstant.SourceFragment.SUG_SEARCH_FRAGMENT;
        }
        return sourceFragment;
    }

    /**
     * 更新搜索历史记录
     * @param historyList 搜索历史记录数据
     */
    public void notifyKeywordRecord(final List<History> historyList) {
        if (!historyList.isEmpty()) {
            mSearchHistoryAdapter.notifyList(historyList);
            mSearchHistoryAdapter.setMIsShowIndex(true);
            mViewBinding.sclDeleteRecordAlong.setVisibility(GONE);
            mViewBinding.rcyRecordAlong.setVisibility(VISIBLE);
            mViewBinding.tvRecordNullAlong.setVisibility(GONE);
        } else {
//            ToastUtils.Companion.getInstance().showCustomToastView(NODATA);
            mViewBinding.sclDeleteRecordAlong.setVisibility(GONE);
            mViewBinding.rcyRecordAlong.setVisibility(GONE);
            mViewBinding.tvRecordNullAlong.setVisibility(VISIBLE);
        }
        mViewBinding.suggestResultListAlong.setVisibility(GONE);

    }

    /**
     * 解析geoPoint字符串
     * @param geoPointString geoPoint字符串
     * @return GeoPoint对象
     */
    private GeoPoint parseGeoPoint(final String geoPointString) {
        final Pattern pattern = Pattern.compile("lon=([-\\d.]+), lat=([-\\d.]+)");
        final Matcher matcher = pattern.matcher(geoPointString);

        double lon = 0.0;
        double lat = 0.0;

        if (matcher.find()) {
            lon = Double.parseDouble(matcher.group(1));
            lat = Double.parseDouble(matcher.group(2));
        } else {
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "parseGeoPoint: No match found for GeoPoint string: "
                    + geoPointString);
        }
        return new GeoPoint(lon, lat);
    }

    /**
     * 获取搜索历史记录列表
     */
    public void getSearchKeywordRecord() {
        mScreenViewModel.getSearchKeywordRecord();
    }

    /**
     * 更新搜索结果
     * @param searchResultEntity 搜索结果数据
     */
    public void notifySearchResult(final SearchResultEntity searchResultEntity) {
        if (!ConvertUtils.isEmpty(mSearchLoadingDialog)) {
            mSearchLoadingDialog.hide();
        }
        if (ConvertUtils.isEmpty(mViewBinding)) {
            return;
        }
        mViewBinding.suggestResultListAlong.setVisibility(VISIBLE);
        mViewBinding.rcyRecordAlong.setVisibility(GONE);
        mViewBinding.sclDeleteRecordAlong.setVisibility(GONE);
        mViewBinding.tvRecordNullAlong.setVisibility(GONE);
        if (searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView(NODATA);
            mSearchLoadingDialog.hide();
            return;
        }
        if (mSearchResultAdapter != null) {
            mSearchResultAdapter.notifyList(searchResultEntity);
        }
    }

    /**
     * 更新静默搜索结果
     * @param searchResultEntity 搜索结果数据
     */
    public void notifySilentSearchResult(final SearchResultEntity searchResultEntity) {

    }

    @HookMethod
    private void sendBuryPointForAddFavorite(final PoiInfoEntity poiInfo, final int type) {
        if (poiInfo != null) {
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
                    .setParams(key, poiInfo.getName())
                    .build();
            BuryPointController.getInstance().setBuryProps(buryProperty);
        }
    }
}
