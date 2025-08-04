package com.sgm.navi.scene.ui.favorite;


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
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.api.search.IOnHomeCompanyClickListener;
import com.sgm.navi.scene.databinding.SceneHomeCompanyViewBinding;
import com.sgm.navi.scene.impl.favorite.SceneHomeCompanyViewImpl;
import com.sgm.navi.scene.impl.search.SearchFragmentFactory;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.Date;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 家、公司、常用地址、收藏地址
 * @CreateDate: $ $
 */
public class SceneHomeCompanyView extends BaseSceneView<SceneHomeCompanyViewBinding, SceneHomeCompanyViewImpl> {

    private static final String DIVIDER = "_";
    private int mHomeCompanyType;
    private IOnHomeCompanyClickListener mClickListener;
    private PoiInfoEntity mUserPoiInfoEntity;
    private boolean mIsClickMyPos = false;

    public void setClickListener(final IOnHomeCompanyClickListener clickListener) {
        this.mClickListener = clickListener;
    }

    public SceneHomeCompanyView(@NonNull final Context context) {
        super(context);
    }

    public SceneHomeCompanyView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneHomeCompanyView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneHomeCompanyViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneHomeCompanyViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneHomeCompanyViewImpl initSceneImpl() {
        return new SceneHomeCompanyViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        if (null != mViewBinding) {
            mViewBinding.setSceneHomeCompanyView(mScreenViewModel);
        }
    }

    @Override
    protected void initObserver() {
        setViewVisibility(AutoMapConstant.HomeCompanyType.COMMON);
        setupSearchActions();
    }

    /**
     * 初始化搜索框相关事件
     */
    private void setupSearchActions() {
        if (null == mViewBinding) {
            return;
        }

        mViewBinding.searchBarTextView.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(final CharSequence s, final int start, final int count, final int after) {

            }

            @Override
            public void onTextChanged(final CharSequence s, final int start, final int before, final int count) {

            }

            @Override
            public void afterTextChanged(final Editable editable) {
                if (!editable.toString().trim().isEmpty()) {
                    mViewBinding.ivEditClear.setVisibility(VISIBLE);
                    suggestionSearch(editable.toString().trim());
                } else {
                    mViewBinding.ivEditClear.setVisibility(GONE);
                }
            }
        });

        mViewBinding.searchBarTextView.setOnEditorActionListener((v, actionId, event) -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onEditorActionListener actionId: " + actionId);
            // 预搜索界面逻辑处理，跳转到搜索结果页面
            final Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                    .navigation();
            final String sourceFragment = getSourceFragment();
            addFragment((BaseFragment) fragment,
                    SearchFragmentFactory.createKeywordFragment(
                            sourceFragment, AutoMapConstant.SearchType.SEARCH_KEYWORD,
                            getEditText(), null));
            hideInput();
            return true;
        });

        mViewBinding.ivEditClear.setOnClickListener(view -> clearEditText());
    }

    /**
     * 获取来源页面
     * @return 来源页面
     */
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
     * 设置页面
     * @param formType 页面类型
     */
    public void setViewVisibility(final int formType) {
        mHomeCompanyType = formType;
        if (null == mViewBinding) {
            return;
        }
        switch (formType) {
            //家和公司: 显示地图选点、我的位置
            case AutoMapConstant.HomeCompanyType.HOME:
                mViewBinding.searchBarTextView.setHint(getContext().getString(R.string.home_search_hint));
                mViewBinding.sclGasStation.setVisibility(View.GONE);
                mViewBinding.sclPullUp.setVisibility(View.GONE);
                mViewBinding.sclCarWashing.setVisibility(View.VISIBLE);
                mViewBinding.sclGourmet.setVisibility(View.VISIBLE);
                break;
            case AutoMapConstant.HomeCompanyType.COMPANY:
                mViewBinding.searchBarTextView.setHint(getContext().getString(R.string.company_search_hint));
                mViewBinding.sclGasStation.setVisibility(View.GONE);
                mViewBinding.sclPullUp.setVisibility(View.GONE);
                mViewBinding.sclCarWashing.setVisibility(View.VISIBLE);
                mViewBinding.sclGourmet.setVisibility(View.VISIBLE);
                break;
            //常用地址:显示收藏夹、收到的点、地图选点
            case AutoMapConstant.HomeCompanyType.COMMON:
                mViewBinding.sclCarWashing.setVisibility(View.GONE);
                mViewBinding.sclGasStation.setVisibility(View.VISIBLE);
                mViewBinding.sclPullUp.setVisibility(View.VISIBLE);
                mViewBinding.sclGourmet.setVisibility(View.VISIBLE);
                break;
            case AutoMapConstant.HomeCompanyType.COLLECTION:
                //收藏夹 显示地图选点，收到的点
                mViewBinding.sclCarWashing.setVisibility(View.GONE);
                mViewBinding.sclGasStation.setVisibility(View.GONE);
                break;
            default:
                break;
        }
    }

    /**
     * 执行关键字搜索
     * @param keyword 关键字
     */
    public void doKeyWordSearch(final String keyword) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "doKeyWordSearch: " + keyword);
        // 语音传入关键字参数时，直接跳转到搜索结果页面
        final Fragment fragment = (Fragment) ARouter.getInstance()
                .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                .navigation();
        final String sourceFragment = getSourceFragment();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createKeywordFragment(
                sourceFragment, AutoMapConstant.SearchType.SEARCH_KEYWORD,
                keyword, null));

    }

    /**
     * 获取输入框内容
     *
     * @return 输入框内容
     */
    private String getEditText() {
        return null != mViewBinding && null != mViewBinding.searchBarTextView.getText() ?
                mViewBinding.searchBarTextView.getText().toString() : "";
    }

    /**
     * 点击文本框弹出键盘
     */
    public void onClickEditText() {
        mViewBinding.searchBarTextView.post(() -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onClickEditText: ");
            mViewBinding.searchBarTextView.requestFocus();
            showKeyboard();
        });
    }

    /**
     * 显示软键盘
     */
    private void showKeyboard() {
        final InputMethodManager imm = (InputMethodManager) getContext().getSystemService(
                Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.showSoftInput(mViewBinding.searchBarTextView, InputMethodManager.SHOW_IMPLICIT);
        }
    }

    /**
     * 请求焦点并显示软键盘
     */
    public void requestFocusAndShowKeyboard() {
        // 确保视图已经附加到窗口
        mViewBinding.searchBarTextView.postDelayed(() -> {
            if (mViewBinding != null) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "requestFocusAndShowKeyboard: ");
                mViewBinding.searchBarTextView.requestFocus();
                showKeyboard();
            }
        }, 300);
    }

    /**
     * 隐藏软键盘
     */
    public void hideInput() {
        final InputMethodManager imm = (InputMethodManager) getContext().getSystemService(
                Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.hideSoftInputFromWindow(getWindowToken(), 0);
        }
    }

    /**
     * 清空输入框内容
     */
    public void clearEditText() {
        mViewBinding.searchBarTextView.setText("");
        if (null != mClickListener) {
            mClickListener.onEditClearClicked();
        }
    }

    /**
     * 执行预搜索
     * @param keyword 关键字
     */
    public void suggestionSearch(final String keyword) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "sugSearch search: " + ", Keyword: " + keyword);
        mScreenViewModel.suggestionSearch(keyword);
    }

    /**
     * 跳转页面
     *
     * @param position 0收藏夹 1收到的点 3地图选点 4我的位置
     */
    public void onClickQuickSearch(final int position) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "homeCompany onClickQuickSearch: position: " + position);
        final Fragment fragment;
        switch (position) {
            case 0: //收藏夹
                //跳转收藏页面
                fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.COLLECT_FRAGMENT)
                        .navigation();
                if (fragment != null) {
                    addFragment((BaseFragment) fragment,
                            SearchFragmentFactory.createCollectFragment(
                                    AutoMapConstant.SourceFragment.FRAGMENT_HOME_COMPANY,
                                    AutoMapConstant.CollectionType.COMMON, mHomeCompanyType));
                }
                break;
            case 1: //收到的点
                fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.COLLECT_FRAGMENT)
                        .navigation();
                if (fragment != null) {
                    addFragment((BaseFragment) fragment,
                            SearchFragmentFactory.createCollectFragment(
                                    AutoMapConstant.SourceFragment.FRAGMENT_HOME_COMPANY,
                                    AutoMapConstant.CollectionType.GET_POINT, mHomeCompanyType));
                }
                break;
            case 2: //地图选点
                //跳转地图选点页面，隐藏所有view
                clickMapPoint();
                break;
            case 3: //我的位置
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "homeCompany onClickQuickSearch: userPoiInfoEntity: "
                        + mUserPoiInfoEntity);
                clickMyPosition();
                break;
            default:
                break;
        }
    }
    /**
     * 跳转地图选点页面
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_MAP_POINT)
    private void clickMapPoint(){
        if (null != mClickListener) {
            mClickListener.setHomeCompanyType(mHomeCompanyType);
            hideInput();
            mScreenViewModel.flyLineVisible(MapType.MAIN_SCREEN_MAIN_MAP, true);
        }
//        closeAllFragmentAndSearchView();
    }

    /**
     * 我的位置
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_MAP_MYLOCATION)
    private void clickMyPosition(){
        mScreenViewModel.currentLocationSearch();
        mIsClickMyPos = true;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        hideInput();
        mIsClickMyPos = false;
    }

    /**
     * 搜索结果回调 添加收藏点时，点击我的位置直接进行收藏操作
     * @param taskId 任务id
     * @param searchResultEntity 搜索结果实体
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        if (!mIsClickMyPos) {
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
        if (searchResultEntity != null
                && !ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
            mUserPoiInfoEntity = searchResultEntity.getPoiList().get(0);
            final int commonName = mHomeCompanyType;
            if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.HOME) {
                ToastUtils.Companion.getInstance().showCustomToastView(
                        ResourceUtils.Companion.getInstance().getString(R.string.mps_set_home_success), 0);
            } else {
                ToastUtils.Companion.getInstance().showCustomToastView(
                        ResourceUtils.Companion.getInstance().getString(R.string.mps_set_company_success), 0);
            }
            final FavoriteInfo favoriteInfo = new FavoriteInfo();
            favoriteInfo.setCommonName(commonName)
                    .setUpdateTime(new Date().getTime());
            mUserPoiInfoEntity.setFavoriteInfo(favoriteInfo);
            if (ConvertUtils.isEmpty(mUserPoiInfoEntity.getPid())) {
                //逆地理搜索出的点无poiId，需自己拼接
                mUserPoiInfoEntity.setPid(mUserPoiInfoEntity.getPoint().getLon() + "_"
                        + mUserPoiInfoEntity.getPoint().getLat());
            }
            BehaviorPackage.getInstance().addFavorite(mUserPoiInfoEntity, commonName);
//            BehaviorPackage.getInstance().addFavoriteData(mUserPoiInfoEntity, commonName);
            SettingUpdateObservable.getInstance().onUpdateSyncTime();
            closeAllFragmentsUntilTargetFragment("HomeCompanyFragment");
            showCurrentFragment();
            mIsClickMyPos = false;
        }
    }

    public void onBackPressed(){
        if(null != mScreenViewModel) mScreenViewModel.closeSearch();
    }
}