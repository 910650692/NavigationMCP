package com.fy.navi.scene.ui.search;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.GridLayoutManager;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.databinding.SceneQuickSearchListBinding;
import com.fy.navi.scene.impl.search.SceneQuickSearchViewImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.QuickSearchListAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @Author: baipeng0904
 * @Description: 快捷搜索 scene，周边搜，沿途搜
 */
public class SceneQuickSearchView extends BaseSceneView<SceneQuickSearchListBinding, SceneQuickSearchViewImpl> {

    private QuickSearchListAdapter quickSearchListAdapter;
    private PoiInfoEntity poiInfoEntity;
    private int searchType;

    public SceneQuickSearchView(@NonNull Context context) {
        super(context);
    }

    public SceneQuickSearchView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneQuickSearchView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneQuickSearchListBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
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
        setupRecyclerView();
        setupAdapterListener();
    }

    private void setupSearchBarCloseAction() {
        mViewBinding.searchTextBarView.ivClose.setOnClickListener(v -> mScreenViewModel.closeSearch());
    }

    private void setupRecyclerView() {
        GridLayoutManager layoutManager = new GridLayoutManager(getContext(), 4);
        layoutManager.setOrientation(GridLayoutManager.VERTICAL);
        mViewBinding.skRcvQuick.setLayoutManager(layoutManager);

        quickSearchListAdapter = new QuickSearchListAdapter();
        mViewBinding.skRcvQuick.setAdapter(quickSearchListAdapter);
    }

    private void setupAdapterListener() {
        if (quickSearchListAdapter != null) {
            quickSearchListAdapter.setOnItemClickListener((position, name) -> {
                Fragment fragment;
                switch (searchType) {
                    case AutoMapConstant.SearchType.AROUND_SEARCH:
                        Logger.d(SEARCH_HMI_TAG, "Around search - Type: " + searchType);
                         fragment= (Fragment) ARouter.getInstance().build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                                .navigation();
                        addFragment((BaseFragment) fragment,SearchFragmentFactory.createKeywordFragment(AutoMapConstant.SourceFragment.FRAGMENT_AROUND, searchType, name, poiInfoEntity));

                        break;
                    case AutoMapConstant.SearchType.ALONG_WAY_SEARCH:
                        Logger.d(SEARCH_HMI_TAG, "Along way search - Type: " + searchType);
                         fragment= (Fragment) ARouter.getInstance().build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                                .navigation();
                        addFragment((BaseFragment) fragment,SearchFragmentFactory.createKeywordFragment(AutoMapConstant.SourceFragment.FRAGMENT_ALONG_WAY, searchType, name, null));
                        break;
                    default:
                        Logger.d(SEARCH_HMI_TAG, "Unsupported search type: " + searchType);
                }
            });
        } else {
            Logger.d(SEARCH_HMI_TAG, "quickSearchListAdapter is null");
        }
    }

    public void setQuickSearchListAdapterData(TypedArray iconArray, String[] categories) {
        if (quickSearchListAdapter != null) {
            quickSearchListAdapter.setCategories(iconArray, categories);
        } else {
            Logger.d(SEARCH_HMI_TAG, "quickSearchListAdapter is null");
        }
    }

    public void setPoiInfoEntity(PoiInfoEntity poiInfoEntity) {
        this.poiInfoEntity = poiInfoEntity;
    }

    public void setTextView(String text) {
        mViewBinding.searchTextBarView.searchBarTextView.setText(text);
    }

    public void setSearchType(int searchType) {
        this.searchType = searchType;
    }
}