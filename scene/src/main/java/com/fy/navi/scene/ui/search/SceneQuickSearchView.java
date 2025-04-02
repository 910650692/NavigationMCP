package com.fy.navi.scene.ui.search;


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
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.databinding.SceneQuickSearchListBinding;
import com.fy.navi.scene.impl.search.SceneQuickSearchViewImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.QuickSearchListAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 快捷搜索 scene，周边搜，沿途搜
 */
public class SceneQuickSearchView extends BaseSceneView<SceneQuickSearchListBinding, SceneQuickSearchViewImpl> {

    private QuickSearchListAdapter mQuickSearchListAdapter;
    private PoiInfoEntity mPoiInfoEntity;
    private int mSearchType;

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
        setupRecyclerView();
        setupAdapterListener();
    }

    /**
     * 关闭搜索框的点击事件
     */
    private void setupSearchBarCloseAction() {
        mViewBinding.searchTextBarView.ivClose.setOnClickListener(v -> mScreenViewModel.closeSearch(mSearchType));
    }

    /**
     * 初始化recyclerView
     */
    private void setupRecyclerView() {
        final GridLayoutManager layoutManager = new GridLayoutManager(getContext(), 4);
        layoutManager.setOrientation(GridLayoutManager.VERTICAL);
        mViewBinding.skRcvQuick.setLayoutManager(layoutManager);

        mQuickSearchListAdapter = new QuickSearchListAdapter();
        mViewBinding.skRcvQuick.setAdapter(mQuickSearchListAdapter);
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
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Around search - Type: " + mSearchType);
                         fragment= (Fragment) ARouter.getInstance().build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                                .navigation();
                         String realName = name;
                         //直接使用维修关键字进行搜索会得到手机电脑维修等搜索结果，需要拼接成汽车维修再进行搜索
                         if (ConvertUtils.equals(name, "维修")) {
                             realName = getContext().getString(R.string.car_repair);
                         }
                        addFragment((BaseFragment) fragment,SearchFragmentFactory.createKeywordFragment(
                                AutoMapConstant.SourceFragment.FRAGMENT_AROUND, mSearchType, realName, mPoiInfoEntity));

                        break;
                    case AutoMapConstant.SearchType.ALONG_WAY_SEARCH:
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Along way search - Type: " + mSearchType);
                        fragment= (Fragment) ARouter.getInstance().build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                                .navigation();
                        addFragment((BaseFragment) fragment,SearchFragmentFactory.createKeywordFragment(
                                AutoMapConstant.SourceFragment.FRAGMENT_ALONG_WAY, mSearchType, name, null));
                        break;
                    default:
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Unsupported search type: " + mSearchType);
                }
            });
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "quickSearchListAdapter is null");
        }
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
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "quickSearchListAdapter is null");
        }
    }

    /**
     * 设置poi信息实体
     * @param poiInfoEntity poi信息实体
     */
    public void setPoiInfoEntity(final PoiInfoEntity poiInfoEntity) {
        this.mPoiInfoEntity = poiInfoEntity;
    }

    /**
     * 设置搜索框的文本
     * @param text 文本
     */
    public void setTextView(final String text) {
        mViewBinding.searchTextBarView.searchBarTextView.setText(text);
    }

    /**
     * 设置搜索类型
     * @param searchType 搜索类型
     */
    public void setSearchType(final int searchType) {
        this.mSearchType = searchType;
    }
}