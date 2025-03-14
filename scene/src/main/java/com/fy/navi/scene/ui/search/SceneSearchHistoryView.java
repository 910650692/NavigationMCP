package com.fy.navi.scene.ui.search;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

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
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.databinding.MainAlongWaySearchHistoryViewBinding;
import com.fy.navi.scene.impl.search.SceneMainAlongWaySearchHistoryImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.SearchHistoryAdapter;
import com.fy.navi.scene.ui.adapter.SearchResultAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @Author: baipeng0904
 * @Description: 沿途主页面历史记录 scene
 * @CreateDate: $ $
 */
public class SceneSearchHistoryView extends BaseSceneView<MainAlongWaySearchHistoryViewBinding, SceneMainAlongWaySearchHistoryImpl> {
    private SearchHistoryAdapter searchHistoryAdapter;
    private SearchResultAdapter searchResultAdapter;
    private SearchLoadingDialog searchLoadingDialog;
    private int homeCompanyType = -1;
    public SceneSearchHistoryView(@NonNull Context context) {
        super(context);
    }

    public SceneSearchHistoryView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);

    }

    public SceneSearchHistoryView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected MainAlongWaySearchHistoryViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
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

    public void setHomeCompanyState(int homeCompanyState) {
        homeCompanyType = homeCompanyState;
        if (searchResultAdapter != null) {
            searchResultAdapter.setHomeCompanyType(homeCompanyState);
        }
        if (searchHistoryAdapter != null) {
            searchHistoryAdapter.setHomeCompanyType(homeCompanyState);
        }
    }

    @Override
    protected void initObserver() {
        Logger.d(SEARCH_HMI_TAG, "SceneSearchHistoryView initObserver");
        searchLoadingDialog = new SearchLoadingDialog(getContext());
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.rcyRecord.setLayoutManager(layoutManager);
        LinearLayoutManager layoutManagerSuggestion = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.suggestResultList.setLayoutManager(layoutManagerSuggestion);

        searchResultAdapter = new SearchResultAdapter();
        mViewBinding.suggestResultList.setAdapter(searchResultAdapter);
        searchHistoryAdapter = new SearchHistoryAdapter();
        mViewBinding.rcyRecord.setAdapter(searchHistoryAdapter);

        getSearchKeywordRecord();

        mViewBinding.sclDeleteRecord.setOnClickListener(view -> {
            if (searchHistoryAdapter.getItemCount() == 0) {
                ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
                return;
            }
            new SearchConfirmDialog.Build(getContext())
                    .setDialogObserver(new IBaseDialogClickListener() {
                        @Override
                        public void onCommitClick() {
                            //清空历史记录
                            SearchPackage.getInstance().clearSearchKeywordRecord();
                            searchHistoryAdapter.notifyList(new ArrayList<>());
                            ToastUtils.Companion.getInstance().showCustomToastView("已清空历史记录");
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
        searchHistoryAdapter.setOnItemClickListener(new SearchHistoryAdapter.ItemClickListener() {
            @Override
            public void onItemClick(int position, History history) {
                Logger.d(SEARCH_HMI_TAG, "BP====onItemClick position:" + position + " history:" + history
                            + " homeCompanyType:" + homeCompanyType);
                if (history.type == AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY) {
                    Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                            .navigation();
                    String sourceFragment;
                    if (homeCompanyType == AutoMapConstant.HomeCompanyType.HOME) {
                        sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_HOME;
                    } else if (homeCompanyType == AutoMapConstant.HomeCompanyType.COMPANY) {
                        sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_COMPANY;
                    } else if (homeCompanyType == AutoMapConstant.HomeCompanyType.COMMON) {
                        sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_COMMON;
                    } else if (homeCompanyType == AutoMapConstant.HomeCompanyType.COLLECTION) {
                        sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION;
                    } else {
                        sourceFragment = AutoMapConstant.SourceFragment.SUG_SEARCH_FRAGMENT;
                    }
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createKeywordFragment(sourceFragment, AutoMapConstant.SearchType.SEARCH_KEYWORD, history.keyWord, null));

                } else {
                    GeoPoint historyPoint = parseGeoPoint(history.endPoint);
                    PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                            .setPid(history.poiId)
                            .setPoint(historyPoint);
                    Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                            .navigation();
                    int poiType = AutoMapConstant.PoiType.POI_KEYWORD;
                    // 1:家 2:公司 3:常用地址 0:收藏夹 -1:都不是
                    if (homeCompanyType == 1) {
                        poiType = AutoMapConstant.PoiType.POI_HOME;
                    } else if (homeCompanyType == 2) {
                        poiType = AutoMapConstant.PoiType.POI_COMPANY;
                    } else if (homeCompanyType == 3) {
                        poiType = AutoMapConstant.PoiType.POI_COMMON;
                    } else if (homeCompanyType == 0) {
                        poiType = AutoMapConstant.PoiType.POI_COLLECTION;
                    }
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, poiType, poiInfoEntity));
                }
            }

            @Override
            public void onNaviClick(int position, History history) {
                PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                poiInfoEntity.setName(history.endPoiName);
                poiInfoEntity.setAddress(history.endPoiName);
                poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
                poiInfoEntity.setPid(history.poiId);
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.smp_set_success), 0);
                int commonName = searchResultAdapter.getHomeCompanyType();
                GeoPoint historyPoint = parseGeoPoint(history.endPoint);
                GeoPoint geoPoint = new GeoPoint();
                geoPoint.setLon(historyPoint.getLon());
                geoPoint.setLat(historyPoint.getLat());
                poiInfoEntity.setPoint(geoPoint);
                FavoriteInfo favoriteInfo = new FavoriteInfo();
                favoriteInfo.setCommonName(commonName)
                        .setItemId(history.poiId + "_" + history.endPoiName + "_" + geoPoint.getLon() + "_" + geoPoint.getLat())
                        .setUpdateTime(new Date().getTime());
                poiInfoEntity.setFavoriteInfo(favoriteInfo);
                if (homeCompanyType == 1
                        || homeCompanyType == 2
                        || homeCompanyType == 3
                        || homeCompanyType == 0) {
                    BehaviorPackage.getInstance().addFavoriteData(poiInfoEntity, commonName);
                    SettingUpdateObservable.getInstance().onUpdateSyncTime();
                    closeAllFragment();
                } else {
                    if (SearchPackage.getInstance().isAlongWaySearch()) {
                        RoutePackage.getInstance().addViaPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
                    } else {
                        SearchPackage.getInstance().clearLabelMark();
                        Fragment fragment = (Fragment) ARouter.getInstance()
                                .build(RoutePath.Route.ROUTE_FRAGMENT)
                                .navigation();
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                    }
                }

            }
        });

        searchResultAdapter.setOnItemClickListener(new SearchResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(int position, PoiInfoEntity poiInfoEntity) {
                Fragment fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                        .navigation();
                int poiType = AutoMapConstant.PoiType.POI_KEYWORD;
                // 1:家 2:公司 3:常用地址 0:收藏夹 -1:都不是
                if (searchResultAdapter.getHomeCompanyType() == 1) {
                    poiType = AutoMapConstant.PoiType.POI_HOME;
                } else if (searchResultAdapter.getHomeCompanyType() == 2) {
                    poiType = AutoMapConstant.PoiType.POI_COMPANY;
                } else if (searchResultAdapter.getHomeCompanyType() == 3) {
                    poiType = AutoMapConstant.PoiType.POI_COMMON;
                } else if (searchResultAdapter.getHomeCompanyType() == 0) {
                    poiType = AutoMapConstant.PoiType.POI_COLLECTION;
                }
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(AutoMapConstant.SourceFragment.SUG_SEARCH_FRAGMENT, poiType, poiInfoEntity));
            }

            @Override
            public void onNaviClick(int position, PoiInfoEntity poiInfoEntity) {
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.smp_set_success), 0);
                int commonName = searchResultAdapter.getHomeCompanyType();
                FavoriteInfo favoriteInfo = new FavoriteInfo();
                favoriteInfo.setCommonName(commonName)
                        .setItemId(poiInfoEntity.getPid() + "_" + poiInfoEntity.getName() + "_" + poiInfoEntity.getPoint().getLon() + "_" + poiInfoEntity.getPoint().getLat())
                        .setUpdateTime(new Date().getTime());
                poiInfoEntity.setFavoriteInfo(favoriteInfo);
                BehaviorPackage.getInstance().addFavoriteData(poiInfoEntity, commonName);
                SettingUpdateObservable.getInstance().onUpdateSyncTime();
                closeAllFragment();
            }
        });
    }

    public void notifyKeywordRecord(List<History> historyList) {
        if (historyList != null) {
            searchHistoryAdapter.notifyList(historyList);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
        }
        mViewBinding.rcyRecord.setVisibility(VISIBLE);
        mViewBinding.suggestResultList.setVisibility(GONE);

    }

    private GeoPoint parseGeoPoint(String geoPointString) {
        Pattern pattern = Pattern.compile("lon=([-\\d.]+), lat=([-\\d.]+)");
        Matcher matcher = pattern.matcher(geoPointString);

        double lon = 0.0;
        double lat = 0.0;

        if (matcher.find()) {
            lon = Double.parseDouble(matcher.group(1));
            lat = Double.parseDouble(matcher.group(2));
        } else {
            Logger.e(SEARCH_HMI_TAG, "parseGeoPoint: No match found for GeoPoint string: " + geoPointString);
        }
        return new GeoPoint(lon, lat);
    }

    public void getSearchKeywordRecord() {
        mScreenViewModel.getSearchKeywordRecord();
    }

    /**
     * 更新搜索结果
     */
    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        if (!ConvertUtils.isEmpty(searchLoadingDialog)) {
            searchLoadingDialog.hide();
        }
        mViewBinding.suggestResultList.setVisibility(VISIBLE);
        mViewBinding.rcyRecord.setVisibility(GONE);
        if (searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            searchLoadingDialog.hide();
            return;
        }
        if (searchResultAdapter != null) {
            searchResultAdapter.notifyList(searchResultEntity);
        }
    }

    /**
     * 更新静默搜索结果
     */
    public void notifySilentSearchResult(SearchResultEntity searchResultEntity) {

    }
}
