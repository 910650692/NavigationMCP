package com.sgm.navi.scene.ui.search;


import android.content.Context;
import android.content.res.TypedArray;
import android.text.Editable;
import android.text.InputType;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;

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
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.databinding.SceneQuickSearchListBinding;
import com.sgm.navi.scene.impl.search.SceneQuickSearchViewImpl;
import com.sgm.navi.scene.impl.search.SearchFragmentFactory;
import com.sgm.navi.scene.ui.adapter.QuickSearchListAdapter;
import com.sgm.navi.scene.ui.adapter.SearchResultAdapter;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.ui.base.BaseFragment;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 快捷搜索 scene，周边搜，沿途搜
 */
public class SceneQuickSearchView extends BaseSceneView<SceneQuickSearchListBinding, SceneQuickSearchViewImpl> {

    private QuickSearchListAdapter mQuickSearchListAdapter;
    private SearchResultAdapter mAdapter;
    private PoiInfoEntity mPoiInfoEntity;
    private int mSearchType;
    private boolean mIsOpenFromNavi;

    public SceneQuickSearchView(@NonNull final Context context) {
        super(context);
    }

    public SceneQuickSearchView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneQuickSearchView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneQuickSearchListBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneQuickSearchListBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneQuickSearchViewImpl initSceneImpl() {
        return new SceneQuickSearchViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setQuickSearchView(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setupSearchBarCloseAction();
        setupSearchActions();
        setupRecyclerView();
        setupAdapterListener();
    }

    /**
     * 关闭搜索框的点击事件
     */
    private void setupSearchBarCloseAction() {
        mViewBinding.searchTextBarView.ivClose.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(final View view) {
                if (mIsOpenFromNavi) {
                    mScreenViewModel.closeSearchOpenFromNavi();
                } else {
                    mScreenViewModel.closeSearch(mSearchType);
                    showCurrentFragment();
                }
            }
        });
    }

    public void closeSearchFragment() {
        if (mIsOpenFromNavi) {
            mScreenViewModel.closeSearchOpenFromNavi();
        } else {
            mScreenViewModel.closeSearch(mSearchType);
            showCurrentFragment();
        }
    }

    /**
     * 初始化recyclerView
     */
    private void setupRecyclerView() {
        final GridLayoutManager layoutManager = new GridLayoutManager(getContext(), 4);
        layoutManager.setOrientation(GridLayoutManager.VERTICAL);
        mViewBinding.skRcvQuick.setLayoutManager(layoutManager);
        if (mQuickSearchListAdapter == null) {
            mQuickSearchListAdapter = new QuickSearchListAdapter();
        }
        mViewBinding.skRcvQuick.setAdapter(mQuickSearchListAdapter);
        final LinearLayoutManager layoutManagerSug = new LinearLayoutManager(getContext());
        layoutManagerSug.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.skRcvSuggestion.setLayoutManager(layoutManagerSug);

        mAdapter = new SearchResultAdapter();
        mViewBinding.skRcvSuggestion.setAdapter(mAdapter);
        mViewBinding.skRcvSuggestion.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrolled(@NonNull final RecyclerView recyclerView, final int dx, final int dy) {
                super.onScrolled(recyclerView, dx, dy);
                if (dy != 0) {
                    //列表滑动时，隐藏软键盘
                    hideInput();
                }
            }
            @Override
            public void onScrollStateChanged(@NonNull final RecyclerView recyclerView, final int newState) {
                super.onScrollStateChanged(recyclerView, newState);
                if (newState == RecyclerView.SCROLL_STATE_DRAGGING) {
                    // 用户正在拖动 RecyclerView,隐藏软键盘
                    hideInput();
                }
            }
        });
        mAdapter.setOnItemClickListener(new SearchResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final int position, final PoiInfoEntity poiInfoEntity) {
                sendBuryPointForHistoryClick(poiInfoEntity.getName());
                final Fragment fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                        .navigation();
                if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                            AutoMapConstant.SourceFragment.FRAGMENT_AROUND, AutoMapConstant.PoiType.POI_AROUND, poiInfoEntity));
                } else {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiType from " , mSearchType);
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                            AutoMapConstant.SourceFragment.FRAGMENT_ALONG_WAY, AutoMapConstant.PoiType.POI_AROUND, poiInfoEntity));
                }
            }

            @Override
            public void onNaviClick(final int position, final PoiInfoEntity poiInfoEntity) {
                if (SearchPackage.getInstance().isAlongWaySearch()) {
                    if (RoutePackage.getInstance().isStartOrEndRouteParam(MapType.MAIN_SCREEN_MAIN_MAP,poiInfoEntity)) {
                        ToastUtils.Companion.getInstance().showCustomToastView(
                                ResourceUtils.Companion.getInstance().getString(R.string.route_error_add_start_end));
                        return;
                    }
                    if (RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
                        RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, true);
                    } else {
                        RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
                    }
                } else {
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Route.ROUTE_FRAGMENT)
                            .navigation();
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                }
            }
        });
    }

    /**
     * 设置adapter的点击事件
     */
    private void setupAdapterListener() {
        if (mQuickSearchListAdapter != null) {
            mQuickSearchListAdapter.setOnItemClickListener((position, name) -> {
                final Fragment fragment;
                switch (mSearchType) {
                    case AutoMapConstant.SearchType.AROUND_SEARCH:
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Around search - Type: " , mSearchType," Name: ", name);
                         fragment= (Fragment) ARouter.getInstance().build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                                .navigation();
                         String realName = name;
                         //直接使用维修关键字进行搜索会得到手机电脑维修等搜索结果，需要拼接成汽车维修再进行搜索
                         if (ConvertUtils.equals(name, "维修")) {
                             realName = getContext().getString(R.string.car_repair);
                         }
                        addFragment((BaseFragment) fragment,SearchFragmentFactory.createKeywordFragment(
                                AutoMapConstant.SourceFragment.FRAGMENT_AROUND, mSearchType, realName, mPoiInfoEntity));

                        //For Bury Point
                        sendBurySearchLocationType(realName);
                        break;
                    case AutoMapConstant.SearchType.ALONG_WAY_SEARCH:
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Along way search - Type: " , mSearchType);
                        String alongRealName = name;
                        //直接使用维修关键字进行搜索会得到手机电脑维修等搜索结果，需要拼接成汽车维修再进行搜索
                        if (ConvertUtils.equals(name, "维修")) {
                            alongRealName = getContext().getString(R.string.car_repair);
                        }
                        //For Bury Point
                        sendBuryPointForAlongWaySearch(alongRealName);

                        fragment= (Fragment) ARouter.getInstance().build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                                .navigation();
                        addFragment((BaseFragment) fragment,SearchFragmentFactory.createKeywordFragment(
                                AutoMapConstant.SourceFragment.FRAGMENT_ALONG_WAY, mSearchType, alongRealName, null));
                        break;
                    default:
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Unsupported search type: " , mSearchType);
                }
            });
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "quickSearchListAdapter is null");
        }
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_MAP_SEARCH_LOCATIONTYPE)
    private void sendBurySearchLocationType(String name) {
        BuryProperty buryParam = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, name)
                .build();
        BuryPointController.getInstance().setBuryProps(buryParam);
    }

    /**
     * 设置快捷搜索列表的数据
     * @param iconArray 图标数组
     * @param categories 类别数组
     */
    public void setQuickSearchListAdapterData(final TypedArray iconArray, final String[] categories) {
        if (mQuickSearchListAdapter != null) {
            mQuickSearchListAdapter.setCategories(iconArray, categories);
        } else {
            mQuickSearchListAdapter = new QuickSearchListAdapter();
            mQuickSearchListAdapter.setCategories(iconArray, categories);
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "quickSearchListAdapter is null");
        }
    }

    /**
     * 设置poi信息实体
     * @param poiInfoEntity poi信息实体
     */
    public void setPoiInfoEntity(final PoiInfoEntity poiInfoEntity) {
        this.mPoiInfoEntity = poiInfoEntity;
        if(!ConvertUtils.isEmpty(poiInfoEntity)){
            mScreenViewModel.createPoiMarker(poiInfoEntity);
        }
    }

    /**
     * 设置搜索框的文本
     * @param text 文本
     */
    public void setTextView(final String text) {
        if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
            mViewBinding.searchTextBarView.searchBarEditView.setText(text);
        } else {
            mViewBinding.searchTextBarView.searchBarEditView.setHint(text);
        }
    }

    /**
     * 设置搜索类型
     * @param searchType 搜索类型
     */
    public void setSearchType(final int searchType) {
        this.mSearchType = searchType;
        //周边搜禁用搜索框点击输入和点击事件，沿途搜时可以点击输入
        if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
            mViewBinding.searchTextBarView.searchBarEditView.setFocusable(false);
            mViewBinding.searchTextBarView.searchBarEditView.setClickable(false);
            mViewBinding.searchTextBarView.searchBarEditView.setInputType(InputType.TYPE_NULL);
            mViewBinding.skRcvSuggestion.setVisibility(GONE);
            mViewBinding.recyclerNoHint.setVisibility(GONE);
            mViewBinding.skRcvQuick.setVisibility(VISIBLE);
        }
    }

    /**
     * @param b 是否从导航进入的搜索页面
     */
    public void setNaviControl(boolean b) {
        mIsOpenFromNavi = b;
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_POI_ROUTE_SELECT)
    private void sendBuryPointForAlongWaySearch(String name) {
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, name)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /**
     * 搜索相关事件
     */
    private void setupSearchActions() {
        if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
            return;
        }
        mViewBinding.searchTextBarView.searchBarEditView.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(final CharSequence s, final int start, final int count, final int after) {

            }

            @Override
            public void onTextChanged(final CharSequence s, final int start, final int before, final int count) {
            }

            @Override
            public void afterTextChanged(final Editable editable) {
                if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                    return;
                }
                if (!editable.toString().trim().isEmpty()) {
                    mViewBinding.searchTextBarView.ivEditClear.setVisibility(VISIBLE);
                    mViewBinding.skRcvSuggestion.setVisibility(VISIBLE);
                    mViewBinding.skRcvQuick.setVisibility(GONE);
                    suggestionSearch(editable.toString().trim());
                } else {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"Text is empty");
                    mViewBinding.searchTextBarView.ivEditClear.setVisibility(GONE);
                    mViewBinding.skRcvSuggestion.setVisibility(GONE);
                    mViewBinding.recyclerNoHint.setVisibility(GONE);
                    mViewBinding.skRcvQuick.setVisibility(VISIBLE);
                }
            }
        });

        mViewBinding.searchTextBarView.searchBarEditView.setOnEditorActionListener((v, actionId, event) -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onEditorActionListener actionId: " + actionId);
            // 预搜索界面逻辑处理，跳转到搜索结果页面
            final Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                    .navigation();
            if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createKeywordFragment(
                        AutoMapConstant.SourceFragment.FRAGMENT_AROUND, mSearchType, getEditText(), null));
            } else {
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createKeywordFragment(
                        AutoMapConstant.SourceFragment.FRAGMENT_ALONG_WAY, mSearchType, getEditText(), null));
            }
            hideInput();
            return true;
        });
        mViewBinding.searchTextBarView.ivEditClear.setOnClickListener(view -> clearEditText());
    }

    /**
     * 清空输入框内容
     */
    public void clearEditText() {
        mViewBinding.searchTextBarView.searchBarEditView.setText("");
        if (null != mAdapter) {
            mAdapter.clearList();
        }
    }

    /**
     * 隐藏软键盘
     */
    public void hideInput() {
        final InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.hideSoftInputFromWindow(getWindowToken(), 0);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        hideInput();
    }

    /**
     * 执行预搜索
     * @param keyword 搜索关键字
     */
    public void suggestionSearch(final String keyword) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "sugSearch search: " + ", Keyword: " + keyword);
        mScreenViewModel.suggestionSearch(keyword);
    }

    /**
     * 更新搜索结果
     * @param taskId 任务id
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        if ((searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) && !getEditText().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            mViewBinding.skRcvSuggestion.setVisibility(GONE);
            mViewBinding.recyclerNoHint.setVisibility(VISIBLE);
            mViewBinding.recyclerNoHint.setText(ResourceUtils.Companion.getInstance().getString(R.string.sug_search_result_no_data));
            mAdapter.clearList();
            return;
        }
        if (ConvertUtils.isEmpty(mScreenViewModel)) {
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "taskId: " + taskId
                + " currentId: " + mScreenViewModel.getMTaskId());
        if (!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) {
            return;
        }
        if (searchResultEntity != null && !ConvertUtils.isEmpty(searchResultEntity.getKeyword())) {
            if (ConvertUtils.isEmpty(getEditText()) || !ConvertUtils.equals(getEditText(), searchResultEntity.getKeyword())) {
                mViewBinding.searchTextBarView.searchBarEditView.setText(searchResultEntity.getKeyword());
            }
        }
        if (mAdapter != null && !getEditText().isEmpty()) {
            mViewBinding.skRcvSuggestion.setVisibility(VISIBLE);
            mViewBinding.recyclerNoHint.setVisibility(GONE);
            mAdapter.notifyList(searchResultEntity);
        }
    }

    /**
     * 获取输入框内容
     *
     * @return 输入框内容
     */
    private String getEditText() {
        return mViewBinding.searchTextBarView.searchBarEditView.getText() != null ?
                mViewBinding.searchTextBarView.searchBarEditView.getText().toString().trim() : "";
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_HISTORY_SELECT)
    private void sendBuryPointForHistoryClick(final String value) {
        final BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, value)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }


}