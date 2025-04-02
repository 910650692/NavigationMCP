package com.fy.navi.scene.ui.search;


import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.databinding.SugSearchResultViewBinding;
import com.fy.navi.scene.impl.search.SceneSugSearchPoiListImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.SearchResultAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 联想搜索结果列表 scene
 * @Date: 2020/4/16 11:05 AM
 * @CreateDate: $ $
 */
public class SceneSugSearchPoiList extends BaseSceneView<SugSearchResultViewBinding, SceneSugSearchPoiListImpl> {
    private SearchResultAdapter mAdapter;

    public SceneSugSearchPoiList(@NonNull final Context context) {
        super(context);
    }

    public SceneSugSearchPoiList(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);

    }

    public SceneSugSearchPoiList(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);

    }

    @Override
    protected SugSearchResultViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SugSearchResultViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneSugSearchPoiListImpl initSceneImpl() {
        return new SceneSugSearchPoiListImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setISceneSugSearchPoiList(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
        setupSearchActions();
        requestFocusAndShowKeyboard();
    }

    /**
     * 初始化 RecyclerView
     */
    private void setupRecyclerView() {
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.recyclerSearchResult.setLayoutManager(layoutManager);

        mAdapter = new SearchResultAdapter();
        mViewBinding.recyclerSearchResult.setAdapter(mAdapter);
        mAdapter.setOnItemClickListener(new SearchResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final int position, final PoiInfoEntity poiInfoEntity) {
                final Fragment fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                        .navigation();
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                        AutoMapConstant.SourceFragment.SUG_SEARCH_FRAGMENT, AutoMapConstant.PoiType.POI_KEYWORD, poiInfoEntity));
            }

            @Override
            public void onNaviClick(final int position, final PoiInfoEntity poiInfoEntity) {
                if (SearchPackage.getInstance().isAlongWaySearch()) {
                    RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
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
     * 搜索相关事件
     */
    private void setupSearchActions() {
        mViewBinding.sclSearchTopView.ivClose.setOnClickListener(v -> mScreenViewModel.closeSearch());

        mViewBinding.sclSearchTopView.searchBarEditView.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(final CharSequence s, final int start, final int count, final int after) {

            }

            @Override
            public void onTextChanged(final CharSequence s, final int start, final int before, final int count) {

            }

            @Override
            public void afterTextChanged(final Editable editable) {
                if (!editable.toString().trim().isEmpty()) {
                    mViewBinding.sclSearchTopView.ivEditClear.setVisibility(VISIBLE);
                    suggestionSearch(editable.toString().trim());
                } else {
                    mViewBinding.sclSearchTopView.ivEditClear.setVisibility(GONE);
                    mAdapter.clearList();
                }
            }
        });

        mViewBinding.sclSearchTopView.searchBarEditView.setOnEditorActionListener((v, actionId, event) -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onEditorActionListener actionId: " + actionId);
            // 预搜索界面逻辑处理，跳转到搜索结果页面
            final Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                    .navigation();
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createKeywordFragment(
                    AutoMapConstant.SourceFragment.SUG_SEARCH_FRAGMENT, AutoMapConstant.SearchType.SEARCH_KEYWORD, getEditText(), null));
            hideInput();
            return true;
        });

        mViewBinding.sclSearchTopView.ivEditClear.setOnClickListener(view -> clearEditText());
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
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final SearchResultEntity searchResultEntity) {
        if (searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            mAdapter.clearList();
            return;
        }
        if (mAdapter != null) {
            mAdapter.notifyList(searchResultEntity);
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

    /**
     * 获取输入框内容
     *
     * @return 输入框内容
     */
    private String getEditText() {
        return mViewBinding.sclSearchTopView.searchBarEditView.getText() != null ?
                mViewBinding.sclSearchTopView.searchBarEditView.getText().toString().trim() : "";
    }

    /**
     * 清空输入框内容
     */
    public void clearEditText() {
        mViewBinding.sclSearchTopView.searchBarEditView.setText("");
        if (null != mAdapter) {
            mAdapter.clearList();
        }
    }

    /**
     * 请求焦点并显示软键盘
     */
    public void requestFocusAndShowKeyboard() {
        // 确保视图已经附加到窗口
        mViewBinding.sclSearchTopView.searchBarEditView.post(() -> {
            mViewBinding.sclSearchTopView.searchBarEditView.requestFocus();
            showKeyboard();
        });
    }

    /**
     * 显示软键盘
     */
    private void showKeyboard() {
        final InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.showSoftInput(mViewBinding.sclSearchTopView.searchBarEditView, InputMethodManager.SHOW_IMPLICIT);
        }
    }
}