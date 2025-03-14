package com.fy.navi.scene.ui.favorite;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.api.search.IOnHomeCompanyClickListener;
import com.fy.navi.scene.databinding.SceneHomeCompanyViewBinding;
import com.fy.navi.scene.impl.favorite.SceneHomeCompanyViewImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.BaseFragment;

import java.util.Date;

/**
 * @Author: baipeng0904
 * @Description: 家、公司、常用地址、收藏地址
 * @CreateDate: $ $
 */
public class SceneHomeCompanyView extends BaseSceneView<SceneHomeCompanyViewBinding, SceneHomeCompanyViewImpl> {
    private int homeCompanyType;
    private IOnHomeCompanyClickListener clickListener;
    private PoiInfoEntity userPoiInfoEntity;
    public void setClickListener(IOnHomeCompanyClickListener clickListener) {
        this.clickListener = clickListener;
    }

    public SceneHomeCompanyView(@NonNull Context context) {
        super(context);
    }

    public SceneHomeCompanyView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneHomeCompanyView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneHomeCompanyViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneHomeCompanyViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneHomeCompanyViewImpl initSceneImpl() {
        return new SceneHomeCompanyViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setSceneHomeCompanyView(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setViewVisibility(AutoMapConstant.HomeCompanyType.COMMON);
        setupSearchActions();
    }

    private void setupSearchActions() {
        mViewBinding.searchBarTextView.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable editable) {
                if (!editable.toString().trim().isEmpty()) {
                    mViewBinding.ivEditClear.setVisibility(VISIBLE);
                    suggestionSearch(editable.toString().trim());
                } else {
                    mViewBinding.ivEditClear.setVisibility(GONE);
                }
            }
        });

        mViewBinding.searchBarTextView.setOnEditorActionListener((v, actionId, event) -> {
            Logger.d(SEARCH_HMI_TAG, "onEditorActionListener actionId: " + actionId);
            // 预搜索界面逻辑处理，跳转到搜索结果页面
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
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createKeywordFragment(sourceFragment, AutoMapConstant.SearchType.SEARCH_KEYWORD, getEditText(), null));
            hideInput();
            return true;
        });
        if (mViewBinding != null && mViewBinding.ivEditClear != null) {
            mViewBinding.ivEditClear.setOnClickListener(view -> clearEditText());
        }
    }

    /**
     * 设置页面
     */
    public void setViewVisibility(int formType) {
        homeCompanyType = formType;
        switch (formType) {
            //家和公司: 显示地图选点、我的位置
            case AutoMapConstant.HomeCompanyType.HOME:
            case AutoMapConstant.HomeCompanyType.COMPANY:
                if (null != mViewBinding.sclGasStation) {
                    mViewBinding.sclGasStation.setVisibility(View.GONE);
                }
                if (null != mViewBinding.sclPullUp) {
                    mViewBinding.sclPullUp.setVisibility(View.GONE);
                }
                if (null != mViewBinding.sclCarWashing) {
                    mViewBinding.sclCarWashing.setVisibility(View.VISIBLE);
                }

                if (null != mViewBinding.sclGourmet) {
                    mViewBinding.sclGourmet.setVisibility(View.VISIBLE);
                }
                break;
            //常用地址:显示收藏夹、收到的点、地图选点
            case AutoMapConstant.HomeCompanyType.COMMON:
                if (null != mViewBinding.sclCarWashing) {
                    mViewBinding.sclCarWashing.setVisibility(View.GONE);
                }
                if (null != mViewBinding.sclGasStation) {
                    mViewBinding.sclGasStation.setVisibility(View.VISIBLE);
                }
                if (null != mViewBinding.sclPullUp) {
                    mViewBinding.sclPullUp.setVisibility(View.VISIBLE);
                }
                if (null != mViewBinding.sclGourmet) {
                    mViewBinding.sclGourmet.setVisibility(View.VISIBLE);
                }
                break;
            case AutoMapConstant.HomeCompanyType.COLLECTION:
                //收藏夹 显示地图选点，收到的点
                if (null != mViewBinding.sclCarWashing) {
                    mViewBinding.sclCarWashing.setVisibility(View.GONE);
                }
                if (null != mViewBinding.sclGasStation) {
                    mViewBinding.sclGasStation.setVisibility(View.GONE);
                }
                break;
        }
    }

    /**
     * 获取输入框内容
     *
     * @return 输入框内容
     */
    private String getEditText() {
        return mViewBinding.searchBarTextView.getText() != null ? mViewBinding.searchBarTextView.getText().toString().trim() : "";
    }

    public void onClickEditText() {
        mViewBinding.searchBarTextView.post(() -> {
            mViewBinding.searchBarTextView.requestFocus();
            showKeyboard();
        });
    }

    /**
     * 显示软键盘
     */
    private void showKeyboard() {
        InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.showSoftInput(mViewBinding.searchBarTextView, InputMethodManager.SHOW_IMPLICIT);
        }
    }

    /**
     * 隐藏软键盘
     */
    public void hideInput() {
        InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.hideSoftInputFromWindow(getWindowToken(), 0);
        }
    }

    /**
     * 清空输入框内容
     */
    public void clearEditText() {
        mViewBinding.searchBarTextView.setText("");
        if (null != clickListener) {
            clickListener.onEditClearClicked();
        }
    }

    /**
     * 执行预搜索
     */
    public void suggestionSearch(String keyword) {
        Logger.d(SEARCH_HMI_TAG, "sugSearch search: " + ", Keyword: " + keyword);
        mScreenViewModel.suggestionSearch(keyword);
    }

    /**
     * 跳转页面
     *
     * @param position 0收藏夹 1收到的点 3地图选点 4我的位置
     */
    public void onClickQuickSearch(int position) {
        Logger.d(SEARCH_HMI_TAG, "homeCompany onClickQuickSearch: position: " + position);
        Fragment fragment;
        switch (position) {
            case 0: //收藏夹
                //跳转收藏页面
                fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.COLLECT_FRAGMENT)
                        .navigation();
                if (fragment != null) {
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createCollectFragment(AutoMapConstant.SourceFragment.FRAGMENT_HOME_COMPANY, AutoMapConstant.CollectionType.COMMON, homeCompanyType));
                }
                break;
            case 1: //收到的点
                fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.COLLECT_FRAGMENT)
                        .navigation();
                if (fragment != null) {
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createCollectFragment(AutoMapConstant.SourceFragment.FRAGMENT_HOME_COMPANY, AutoMapConstant.CollectionType.GET_POINT, homeCompanyType));
                }
                break;
            case 2: //地图选点
                //跳转地图选点页面，隐藏所有view
                if (null != clickListener) {
                    clickListener.setHomeCompanyType(homeCompanyType);
                }
                closeAllFragmentAndSearchView();
                break;
            case 3: //我的位置
                Logger.d(SEARCH_HMI_TAG, "homeCompany onClickQuickSearch: userPoiInfoEntity: " + userPoiInfoEntity);
                SearchPackage.getInstance().currentLocationSearch();
                break;
        }
    }
    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        if (searchResultEntity != null
                && searchResultEntity.getPoiList() != null
                && searchResultEntity.getPoiList().size() > 0) {
            userPoiInfoEntity = searchResultEntity.getPoiList().get(0);
            int commonName = homeCompanyType;
            if (homeCompanyType == AutoMapConstant.HomeCompanyType.HOME) {
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.mps_set_home_success), 0);
            } else {
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.mps_set_company_success), 0);
            }
            FavoriteInfo favoriteInfo = new FavoriteInfo();
            favoriteInfo.setCommonName(commonName)
                    .setItemId(userPoiInfoEntity.getPid() + "_" + userPoiInfoEntity.getName() + "_" + userPoiInfoEntity.getPoint().getLon() + "_" + userPoiInfoEntity.getPoint().getLat())
                    .setUpdateTime(new Date().getTime());
            userPoiInfoEntity.setFavoriteInfo(favoriteInfo);
            BehaviorPackage.getInstance().addFavoriteData(userPoiInfoEntity, commonName);
            SettingUpdateObservable.getInstance().onUpdateSyncTime();
            closeAllFragment();
        }
    }
}