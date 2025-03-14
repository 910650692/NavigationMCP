package com.fy.navi.scene.ui.search;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.adapter.GasStationAdapter;
import com.fy.navi.scene.adapter.GridSpacingItemDecoration;
import com.fy.navi.scene.adapter.HorizontalSpaceItemDecoration;
import com.fy.navi.scene.api.search.IOnFilterItemClickListener;
import com.fy.navi.scene.databinding.PoiSearchResultViewBinding;
import com.fy.navi.scene.impl.search.SceneSearchPoiListImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.CollectResultAdapter;
import com.fy.navi.scene.ui.adapter.FilterChildListAdapter;
import com.fy.navi.scene.ui.adapter.FilterListAdapter;
import com.fy.navi.scene.ui.adapter.SearchResultAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchCategoryLocalInfo;
import com.fy.navi.service.define.search.SearchChildCategoryLocalInfo;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.view.refresh.RefreshListener;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * @Author: baipeng0904
 * @Description: 搜索结果列表scene
 * @Date: 2020/4/16 11:05 AM
 * @CreateDate: $ $
 */
public class SceneSearchPoiList extends BaseSceneView<PoiSearchResultViewBinding, SceneSearchPoiListImpl> {
    private SearchResultAdapter mAdapter;
    private FilterListAdapter filterOneAdapter;
    private FilterListAdapter filterTwoAdapter;
    private FilterListAdapter filterThreeAdapter;
    private FilterChildListAdapter filterOneChildAdapter;
    private FilterChildListAdapter filterTwoChildAdapter;
    private FilterChildListAdapter filterThreeChildAdapter;
    private int maxPageNum = 1;
    private int pageNum = 1;
    private SearchLoadingDialog searchLoadingDialog;
    private int searchType = AutoMapConstant.SearchType.SEARCH_KEYWORD;
    private String searchText;
    private PoiInfoEntity poiInfoEntity;
    private SearchResultEntity resultEntity;
    private List<SearchCategoryLocalInfo> localInfoList;
    private boolean isFilterViewShow = false;
    private int horizontalSpacing = 12;
    private int childHorizontalSpacing = 16;
    private int childVerticalSpacing = 16;
    private int spanCount = 3;
    //第一个一级菜单当前正在被选中的二级菜单下标
    private int currentSelectedIndex1 = -1;
    //第二个一级菜单当前正在被选中的二级菜单下标
    private int currentSelectedIndex2 = -1;
    //第三个一级菜单当前正在被选中的二级菜单下标
    private int currentSelectedIndex3 = -1;
    private int homeCompanyType;
    //已下载的城市列表
    private ArrayList<ProvDataInfo> mProvDataInfos;


    public SceneSearchPoiList(@NonNull Context context) {
        super(context);
    }

    public SceneSearchPoiList(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneSearchPoiList(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected PoiSearchResultViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return PoiSearchResultViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneSearchPoiListImpl initSceneImpl() {
        return new SceneSearchPoiListImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setIScenePoiList(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
        setupSearchActions();
        setupRefreshListener();
        setupFilterActions();
        setupChildListActions();
        searchLoadingDialog = new SearchLoadingDialog(getContext());
    }

    /**
     * 初始化 RecyclerView
     */
    private void setupRecyclerView() {
        mViewBinding.recyclerSearchResult.setLayoutManager(new LinearLayoutManager(getContext()));
        mAdapter = new SearchResultAdapter();
        mViewBinding.recyclerSearchResult.setAdapter(mAdapter);

        RecyclerView.ItemDecoration itemDecoration = new HorizontalSpaceItemDecoration(horizontalSpacing);
        mViewBinding.searchFilterView.searchFilterList1.setLayoutManager(new LinearLayoutManager(getContext(), RecyclerView.HORIZONTAL, false));
        filterOneAdapter = new FilterListAdapter();
        mViewBinding.searchFilterView.searchFilterList1.setAdapter(filterOneAdapter);
        mViewBinding.searchFilterView.searchFilterList1.addItemDecoration(itemDecoration);

        mViewBinding.searchFilterView.searchFilterList2.setLayoutManager(new LinearLayoutManager(getContext(), RecyclerView.HORIZONTAL, false));
        filterTwoAdapter = new FilterListAdapter();
        mViewBinding.searchFilterView.searchFilterList2.setAdapter(filterTwoAdapter);
        mViewBinding.searchFilterView.searchFilterList2.addItemDecoration(itemDecoration);

        mViewBinding.searchFilterView.searchFilterList3.setLayoutManager(new LinearLayoutManager(getContext(), RecyclerView.HORIZONTAL, false));
        filterThreeAdapter = new FilterListAdapter();
        mViewBinding.searchFilterView.searchFilterList3.setAdapter(filterThreeAdapter);
        mViewBinding.searchFilterView.searchFilterList3.addItemDecoration(itemDecoration);

        RecyclerView.ItemDecoration gridDecoration = new GridSpacingItemDecoration(getContext(), spanCount, childVerticalSpacing, childHorizontalSpacing, false);
        mViewBinding.searchFilterView.searchFilterList1Child.setLayoutManager(new GridLayoutManager(getContext(), spanCount));
        filterOneChildAdapter = new FilterChildListAdapter();
        mViewBinding.searchFilterView.searchFilterList1Child.setAdapter(filterOneChildAdapter);
        mViewBinding.searchFilterView.searchFilterList1Child.addItemDecoration(gridDecoration);

        mViewBinding.searchFilterView.searchFilterList2Child.setLayoutManager(new GridLayoutManager(getContext(), spanCount));
        filterTwoChildAdapter = new FilterChildListAdapter();
        mViewBinding.searchFilterView.searchFilterList2Child.setAdapter(filterTwoChildAdapter);
        mViewBinding.searchFilterView.searchFilterList2Child.addItemDecoration(gridDecoration);

        mViewBinding.searchFilterView.searchFilterList3Child.setLayoutManager(new GridLayoutManager(getContext(), spanCount));
        filterThreeChildAdapter = new FilterChildListAdapter();
        mViewBinding.searchFilterView.searchFilterList3Child.setAdapter(filterThreeChildAdapter);
        mViewBinding.searchFilterView.searchFilterList3Child.addItemDecoration(gridDecoration);

        mAdapter.setOnItemClickListener(new SearchResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(int position, PoiInfoEntity poiInfoEntity) {
                Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Search.POI_DETAILS_FRAGMENT).navigation();
                int poiType = AutoMapConstant.PoiType.POI_KEYWORD;
                // 1:家 2:公司 3:常用地址 0:收藏夹 -1:都不是
                if (mAdapter.getHomeCompanyType() == 1) {
                    poiType = AutoMapConstant.PoiType.POI_HOME;
                } else if (mAdapter.getHomeCompanyType() == 2) {
                    poiType = AutoMapConstant.PoiType.POI_COMPANY;
                } else if (mAdapter.getHomeCompanyType() == 3) {
                    poiType = AutoMapConstant.PoiType.POI_COMMON;
                } else if (mAdapter.getHomeCompanyType() == 0) {
                    poiType = AutoMapConstant.PoiType.POI_COLLECTION;
                }
                Logger.d(SEARCH_HMI_TAG, "onClick poiType: " + poiType + " homeCompany: " + mAdapter.getHomeCompanyType());
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(AutoMapConstant.SourceFragment.SEARCH_RESULT_FRAGMENT, poiType, poiInfoEntity));
            }

            @Override
            public void onNaviClick(int position, PoiInfoEntity poiInfoEntity) {
                Logger.d(SEARCH_HMI_TAG, "onNaviClick: "+mAdapter.getHomeCompanyType());
                if (mAdapter.getHomeCompanyType() == 1
                        || mAdapter.getHomeCompanyType() == 2
                        || mAdapter.getHomeCompanyType() == 3
                        || mAdapter.getHomeCompanyType() == 0) {
                    ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.smp_set_success), 0);
                    int commonName = mAdapter.getHomeCompanyType();
                    FavoriteInfo favoriteInfo = new FavoriteInfo();
                    favoriteInfo.setCommonName(commonName)
                            .setItemId(poiInfoEntity.getPid() + "_" + poiInfoEntity.getName() + "_" + poiInfoEntity.getPoint().getLon() + "_" + poiInfoEntity.getPoint().getLat())
                            .setUpdateTime(new Date().getTime());
                    poiInfoEntity.setFavoriteInfo(favoriteInfo);
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
    }

    /**
     * 配置搜索相关事件
     */
    private void setupSearchActions() {
        mViewBinding.searchTextBarView.ivClose.setOnClickListener(v -> mScreenViewModel.closeSearch());
        mViewBinding.searchTextBarView.searchBarTextView.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "setupSearchActions: " + searchType);
            if (searchType == AutoMapConstant.SearchType.SEARCH_KEYWORD) {
                mScreenViewModel.closeSearch();
            }

        });
    }

    private void setupFilterActions() {
        mViewBinding.searchFilterView.searchFilterRoot.setVisibility(GONE);
        mViewBinding.searchFilterView.searchFilterConfirm.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "click confirm: ");
            mViewBinding.searchFilterView.searchFilterRoot.setVisibility(GONE);
            mViewBinding.pullRefreshLayout.setVisibility(VISIBLE);
            isFilterViewShow = false;
            mViewBinding.searchTextBarView.searchBarTextView.setText(searchText);
            mScreenViewModel.keywordSearch(pageNum, searchText, resultEntity.getRetain(), "1", getClassifyData(), false);
        });
        mViewBinding.searchFilterView.searchFilterCancel.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "click reset: ");
            mViewBinding.searchFilterView.searchFilterRoot.setVisibility(GONE);
            mViewBinding.pullRefreshLayout.setVisibility(VISIBLE);
            isFilterViewShow = false;
            mViewBinding.searchTextBarView.searchBarTextView.setText(searchText);
            mScreenViewModel.keywordSearch(pageNum, searchText);
        });
        mViewBinding.searchTextBarView.csFilter.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "click filter: ");
            if (!isFilterViewShow) {
                mViewBinding.searchFilterView.searchFilterRoot.setVisibility(VISIBLE);
                mViewBinding.pullRefreshLayout.setVisibility(GONE);
                mViewBinding.searchResultNoData.setVisibility(GONE);
                isFilterViewShow = true;
                if (null != resultEntity) {
                    mViewBinding.searchTextBarView.searchBarTextView.setText(getContext().getString(R.string.filter_result, searchText, resultEntity.getTotal()));
                }
            }
            if (!localInfoList.isEmpty()) {
                for (int i = 0; i < localInfoList.size(); i++) {
                    SearchCategoryLocalInfo searchCategoryLocalInfo = localInfoList.get(i);
                    if (i == 0) {
                        if (ConvertUtils.isEmpty(searchCategoryLocalInfo.getCategoryLocalInfos())) {
                            mViewBinding.searchFilterView.searchFilterTitle1.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList1.setVisibility(GONE);
                        }
                        mViewBinding.searchFilterView.searchFilterTitle1.setText(searchCategoryLocalInfo.getName());
                        filterOneAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos());
                    } else if (i == 1) {
                        if (ConvertUtils.isEmpty(searchCategoryLocalInfo.getCategoryLocalInfos())) {
                            mViewBinding.searchFilterView.searchFilterTitle2.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList2.setVisibility(GONE);
                        }
                        mViewBinding.searchFilterView.searchFilterTitle2.setText(searchCategoryLocalInfo.getName());
                        filterTwoAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos());
                    } else if (i == 2) {
                        if (ConvertUtils.isEmpty(searchCategoryLocalInfo.getCategoryLocalInfos())) {
                            mViewBinding.searchFilterView.searchFilterTitle3.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList3.setVisibility(GONE);
                        }
                        mViewBinding.searchFilterView.searchFilterTitle3.setText(searchCategoryLocalInfo.getName());
                        filterThreeAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos());
                    }
                }
            }
        });

        filterOneAdapter.setFilterItemClickListener(new IOnFilterItemClickListener() {
            @Override
            public void onItemClick(int position) {
                currentSelectedIndex1 = position;
                refreshLocalInfoListCheckedState(0, currentSelectedIndex1);
                filterOneChildAdapter.setCategoryList(null);
                mScreenViewModel.keywordSearch(pageNum, searchText, resultEntity.getRetain(), "1", getClassifyData(), true);
            }

            @Override
            public void onChildListExpandCollapse(List<SearchChildCategoryLocalInfo> childList, int position) {
                currentSelectedIndex1 = position;
                refreshLocalInfoListCheckedState(0, currentSelectedIndex1);
                filterOneChildAdapter.setCategoryList(childList);
            }
        });

        filterTwoAdapter.setFilterItemClickListener(new IOnFilterItemClickListener() {
            @Override
            public void onItemClick(int position) {
                currentSelectedIndex2 = position;
                refreshLocalInfoListCheckedState(1, currentSelectedIndex2);
                filterTwoChildAdapter.setCategoryList(null);
                mScreenViewModel.keywordSearch(pageNum, searchText, resultEntity.getRetain(), "1", getClassifyData(), true);
            }

            @Override
            public void onChildListExpandCollapse(List<SearchChildCategoryLocalInfo> childList, int position) {
                currentSelectedIndex2 = position;
                refreshLocalInfoListCheckedState(1, currentSelectedIndex2);
                filterTwoChildAdapter.setCategoryList(childList);
            }
        });

        filterThreeAdapter.setFilterItemClickListener(new IOnFilterItemClickListener() {
            @Override
            public void onItemClick(int position) {
                currentSelectedIndex3 = position;
                refreshLocalInfoListCheckedState(2, currentSelectedIndex3);
                filterThreeChildAdapter.setCategoryList(null);
                mScreenViewModel.keywordSearch(pageNum, searchText, resultEntity.getRetain(), "1", getClassifyData(), true);
            }

            @Override
            public void onChildListExpandCollapse(List<SearchChildCategoryLocalInfo> childList, int position) {
                currentSelectedIndex3 = position;
                refreshLocalInfoListCheckedState(2, currentSelectedIndex3);
                filterThreeChildAdapter.setCategoryList(childList);
            }
        });
    }

    private void setupChildListActions() {
        filterOneChildAdapter.setFilterItemClickListener(position -> {
            refreshLocalInfoListCheckedState(0, currentSelectedIndex1);
            filterOneAdapter.notifyDataSetChanged();
            mScreenViewModel.keywordSearch(pageNum, searchText, resultEntity.getRetain(), "1", getClassifyData(), true);
        });

        filterTwoChildAdapter.setFilterItemClickListener(position -> {
            refreshLocalInfoListCheckedState(1, currentSelectedIndex2);
            filterOneAdapter.notifyDataSetChanged();
            mScreenViewModel.keywordSearch(pageNum, searchText, resultEntity.getRetain(), "1", getClassifyData(), true);
        });

        filterThreeChildAdapter.setFilterItemClickListener(position -> {
            refreshLocalInfoListCheckedState(2, currentSelectedIndex3);
            filterOneAdapter.notifyDataSetChanged();
            mScreenViewModel.keywordSearch(pageNum, searchText, resultEntity.getRetain(), "1", getClassifyData(), true);
        });
    }

    private void refreshLocalInfoListCheckedState(int index1, int index2) {
        if (localInfoList != null && !localInfoList.isEmpty() && index1 < localInfoList.size() - 1) {
            SearchCategoryLocalInfo categoryLocalInfo = localInfoList.get(index1);
            for (int i = 0; i < categoryLocalInfo.getCategoryLocalInfos().size(); i++) {
                SearchChildCategoryLocalInfo childInfo = categoryLocalInfo.getCategoryLocalInfos().get(i);
                if (i == index2) {
                    childInfo.setChecked(1);
                } else {
                    childInfo.setChecked(-1);
                    for (SearchChildCategoryLocalInfo child : childInfo.getCategoryLocalInfos()) {
                        child.setChecked(-1);
                    }
                }
            }
        }
    }

    /**
     * 设置刷新监听
     */
    private void setupRefreshListener() {
        mViewBinding.pullRefreshLayout.setRefreshListener(new RefreshListener() {
            @Override
            public void refresh() {
                if (pageNum == 1) {
                    Logger.d(SEARCH_HMI_TAG, "已经是第一页，无法刷新");
                } else {
                    performSearch(--pageNum, getEditText());
                }
                mViewBinding.pullRefreshLayout.finishRefresh();
            }

            @Override
            public void loadMore() {
                if (pageNum >= maxPageNum) {
                    Logger.d(SEARCH_HMI_TAG, "没有更多数据了，pageNum: " + pageNum + " / maxPageNum: " + maxPageNum);
                } else {
                    performSearch(++pageNum, getEditText());
                }
                mViewBinding.pullRefreshLayout.finishLoadMore();
            }
        });
    }

    /**
     * 关键字搜索
     */
    public void performSearch(int pageNum, String keyword) {
        if (keyword == null || keyword.trim().isEmpty()) {
            Logger.w(SEARCH_HMI_TAG, "搜索关键字为空，取消搜索");
            return;
        }

        Logger.d(SEARCH_HMI_TAG, "执行搜索 - 类型: " + searchType + ", 关键字: " + keyword + ", 页码: " + pageNum);
        searchLoadingDialog.show();

        switch (searchType) {
            case AutoMapConstant.SearchType.SEARCH_KEYWORD:
                mScreenViewModel.keywordSearch(pageNum, keyword);
                break;
            case AutoMapConstant.SearchType.AROUND_SEARCH:
                mScreenViewModel.aroundSearch(pageNum, keyword, poiInfoEntity);
                break;
            case AutoMapConstant.SearchType.ALONG_WAY_SEARCH:
                mScreenViewModel.alongWaySearch(keyword);
                break;
            default:
                Logger.w(SEARCH_HMI_TAG, "未知搜索类型: " + searchType);
        }
    }

    /**
     * 设置搜索框文本并进行搜索
     */
    public void setEditText(int searchType, String searchText) {
        Logger.d(SEARCH_HMI_TAG, "设置搜索框文本并进行搜索 - 类型: " + searchType + ", 关键字: " + searchText);
        this.searchType = searchType;
        this.searchText = searchText;
        mViewBinding.searchTextBarView.searchBarTextView.setText(searchText);
        this.pageNum = 1;
        performSearch(pageNum, searchText);
    }

    public void setPoiInfoEntity(PoiInfoEntity poiInfo) {
        this.poiInfoEntity = poiInfo;
    }

    public void setHomeCompanyState(int homeCompanyState) {
        this.homeCompanyType = homeCompanyState;
        Logger.d(SEARCH_HMI_TAG, "setHomeCompanyState - homeCompanyState: " + homeCompanyState);
        if (mAdapter != null) {
            mAdapter.setHomeCompanyType(homeCompanyState);
        }
    }

    /**
     * 更新搜索结果
     */
    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        if (!ConvertUtils.isEmpty(searchLoadingDialog)) {
            searchLoadingDialog.hide();
        }
        if (searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("抱歉，未找到结果");
            searchLoadingDialog.hide();
            if (null != mAdapter) {
                mAdapter.clearList();
            }
            if (searchResultEntity.getPoiType() == 0) {
                //离线搜索无数据时，跳转城市列表搜索界面
                Fragment fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.OFFLINE_SEARCH_FRAGMENT)
                        .navigation();
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createOfflineFragment(searchText));
            }
            //搜索无数据时，展示无结果页面
            mViewBinding.searchResultNoData.setVisibility(VISIBLE);
            return;
        }
        //有数据时，隐藏异常提示界面
        mViewBinding.searchResultNoData.setVisibility(GONE);

        if (searchResultEntity.getPoiType() == 0) {
            CityDataInfo cityDataInfo = mScreenViewModel.getCityInfo(mScreenViewModel.getAcCode());
            if (cityDataInfo != null) {
                Logger.d(SEARCH_HMI_TAG, "城市数据信息: " + cityDataInfo.name + "，城市编码: " + mScreenViewModel.getAcCode());
                mViewBinding.searchOfflineHint.setVisibility(VISIBLE);
                mViewBinding.searchOfflineHint.setText(getContext().getString(R.string.search_offline_hint, cityDataInfo.name));
            }
        }
        resultEntity = searchResultEntity;
        searchType = searchResultEntity.getSearchType();
        if (!ConvertUtils.isEmpty(searchResultEntity.getPoiList()) && searchResultEntity.getPoiList().size() == 1) {
            //只有一个搜索结果时，直接跳转结果界面
            Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Search.POI_DETAILS_FRAGMENT).navigation();
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
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(AutoMapConstant.SourceFragment.SEARCH_RESULT_FRAGMENT, poiType, searchResultEntity.getPoiList().get(0)));
            return;
        }
        setMaxPageNum(searchResultEntity.getMaxPageNum());
        if (mAdapter != null) {
            mAdapter.notifyList(searchResultEntity);
            mViewBinding.recyclerSearchResult.scrollToPosition(0);
        }
        if (isFilterViewShow) {
            mViewBinding.searchTextBarView.searchBarTextView.setText(getContext().getString(R.string.filter_result, searchText, searchResultEntity.getTotal()));
        }
        localInfoList = searchResultEntity.getLocalInfoList();
        if (!ConvertUtils.isEmpty(localInfoList)) {
            mViewBinding.searchTextBarView.csFilter.setVisibility(VISIBLE);
            mViewBinding.searchTextBarView.ivSearch.setVisibility(GONE);
        } else {
            mViewBinding.searchTextBarView.csFilter.setVisibility(GONE);
        }
    }

    /**
     * 静默搜索,仅用于显示泛搜结果数量,不进行页面更新
     *
     * @param searchResultEntity 数据实体类
     */
    public void notifySilentSearchResult(SearchResultEntity searchResultEntity) {
        if (!ConvertUtils.isEmpty(searchLoadingDialog)) {
            searchLoadingDialog.hide();
        }
        if (searchResultEntity == null || searchResultEntity.getPoiList() == null || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            mViewBinding.searchTextBarView.searchBarTextView.setText(getContext().getString(R.string.filter_result, searchText, 0));
            searchLoadingDialog.hide();
            return;
        }
        if (isFilterViewShow) {
            Logger.d(SEARCH_HMI_TAG, "notifySilentSearchResult total: " + searchResultEntity.getTotal());
            mViewBinding.searchTextBarView.searchBarTextView.setText(getContext().getString(R.string.filter_result, searchText, searchResultEntity.getTotal()));
        }
    }

    private void setMaxPageNum(int maxPageNum) {
        this.maxPageNum = maxPageNum;
        Logger.d(SEARCH_HMI_TAG, "更新 maxPageNum: " + maxPageNum);
    }

    /**
     * 获取输入框内容
     */
    private String getEditText() {
        CharSequence text = mViewBinding.searchTextBarView.searchBarTextView.getText();
        return text != null ? text.toString().trim() : "";
    }

    /**
     * 清空输入框内容
     */
    public void clearEditText() {
        mViewBinding.searchTextBarView.searchBarTextView.setText("");
    }


    /**
     * 是否需要添加途经点
     *
     * @return true: 需要添加途经点，false: 不需要添加途经点
     */
    public boolean isAlongWaySearch() {
        return mScreenViewModel.isAlongWaySearch();
    }

    private String getClassifyData() {
        StringBuilder stringBuilder = new StringBuilder();
        for (SearchCategoryLocalInfo searchCategoryLocalInfo : localInfoList) {
            for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo : searchCategoryLocalInfo.getCategoryLocalInfos()) {
                if (searchChildCategoryLocalInfo.getChecked() == 1
                        && !ConvertUtils.isEmpty(searchChildCategoryLocalInfo.getValue())) {
                    stringBuilder.append(searchChildCategoryLocalInfo.getValue());
                    stringBuilder.append("+");
                }
                for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo1 : searchChildCategoryLocalInfo.getCategoryLocalInfos()) {
                    if (searchChildCategoryLocalInfo1.getChecked() == 1
                            && !ConvertUtils.isEmpty(searchChildCategoryLocalInfo1.getValue())) {
                        stringBuilder.append(searchChildCategoryLocalInfo1.getValue());
                        stringBuilder.append("+");
                    }
                }
            }
        }
        if (stringBuilder.toString().endsWith("+")) {
            stringBuilder.deleteCharAt(stringBuilder.length() - 1);
        }
        Logger.d(SEARCH_HMI_TAG, "mSearchCategoryLocalInfos getClassifyData: " + stringBuilder.toString());
        return stringBuilder.toString();
    }

    public void clear() {
        poiInfoEntity = null;
        resultEntity = null;
        if (localInfoList != null) {
            localInfoList.clear();
            localInfoList = null;
        }
    }
}