package com.sgm.navi.ui.base;

import android.content.res.Configuration;
import android.os.Build;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.databinding.DataBindingUtil;
import androidx.databinding.ViewDataBinding;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentTransaction;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.ViewModelProvider;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.screen.ScreenTypeUtils;
import com.android.utils.theme.ThemeType;
import com.android.utils.theme.ThemeUtils;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;

public abstract class BaseActivity<V extends ViewDataBinding, VM extends BaseViewModel>
        extends AppCompatActivity implements IBaseView {
    private static final String KEY_CHANGE_SAVE_INSTANCE = "key_change_save_instance";
    private final String LIFE_CYCLE_TAG = "activity_life_cycle";
    protected StackManager mStackManager;
    protected V mBinding;
    protected VM mViewModel;
    protected String mScreenId;
    private String mLastClosedFragmentName;
    public boolean isMapActivity = false;
    private int recreateFrom = 0;
    private int currentUiMode = 0;

    public BaseActivity() {
        super();
        Logger.i(LIFE_CYCLE_TAG, this + " onCreate before");
        mStackManager = StackManager.getInstance();
        onCreateBefore();
        mStackManager.push(mScreenId, this);
    }

    @Override
    protected void onCreate(final @Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Logger.i(LIFE_CYCLE_TAG, this + " onCreate start");
        setImmersiveStatusBar();
        createViewModel();
        boolean isBindView = onCreateViewBefore();
        if (!isBindView) return;
        bindContentView();
        onInitView();
        onInitObserver();
        onInitData();
        Logger.i(LIFE_CYCLE_TAG, this + "onCreate end");
        currentUiMode = ThemeUtils.INSTANCE.getSystemCurrentUiMode(getApplicationContext());
        onRestoreInstance(savedInstanceState);
    }

    @Override
    protected void onResume() {
        super.onResume();
        Logger.i(LIFE_CYCLE_TAG, this + " onResume");
    }

    @Override
    protected void onStart() {
        super.onStart();
        Logger.i(LIFE_CYCLE_TAG, this + " onStart");
    }

    @Override
    protected void onPause() {
        super.onPause();
        Logger.i(LIFE_CYCLE_TAG, this + " onPause");
    }

    @Override
    protected void onStop() {
        super.onStop();
        Logger.i(LIFE_CYCLE_TAG, this + " onStop");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        Logger.i(LIFE_CYCLE_TAG, this + " onDestroy");
        mStackManager.removeBaseView(mScreenId, this);
    }

    @Override
    public void finish() {
        super.finish();
        Logger.i(LIFE_CYCLE_TAG, this + " finish");
        mStackManager.popActivity(mScreenId);
    }

    @Override
    public void onBackPressed() {
        super.onBackPressed();
        Logger.i(LIFE_CYCLE_TAG, this + " 阻止返回键");
    }

    @Override
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        // TODO: 2025/7/13 这句log很重要，切勿删除
        Logger.i(LIFE_CYCLE_TAG, "InstanceState " + this, recreateFrom
                , "task id", this.getTaskId()
                , "current_stack_fragment", mStackManager.getFragmentSize(mScreenId)
                , mStackManager.getCurrentActivity(mScreenId).toString()
                , "current_activity_stack", mStackManager.getActivitySize(mScreenId));
        outState.putInt(KEY_CHANGE_SAVE_INSTANCE, recreateFrom);
    }

    @Override
    public void onRestoreInstance(Bundle savedInstanceState) {
        // TODO: 2025/7/13 这些log很重要，切勿删除
        Logger.i(LIFE_CYCLE_TAG, "InstanceState " + this, this.getTaskId());
        if (!ConvertUtils.isEmpty(savedInstanceState)) {
            int isNeedToUpdateData = savedInstanceState.getInt(KEY_CHANGE_SAVE_INSTANCE);
            if (ThemeType.getThemeValueByType(ThemeType.NIGHT) == isNeedToUpdateData
                    || ThemeType.getThemeValueByType(ThemeType.DAY) == isNeedToUpdateData) {
                Logger.i(LIFE_CYCLE_TAG, "InstanceState " + this, "update Map Theme", isNeedToUpdateData);
                onApplySkin(ThemeType.getThemeTypeByValue(isNeedToUpdateData), false);
            }
        }
        refreshFragment();
    }

    @Override
    public void onApplySkin(ThemeType type, boolean screenType) {
        Logger.i(LIFE_CYCLE_TAG, "onApplySkin " + this, "Theme: ", type);
        recreateFrom = 0;
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        int newUiMode = newConfig.uiMode & Configuration.UI_MODE_NIGHT_MASK;
        Logger.i(LIFE_CYCLE_TAG, "onConfigurationChanged " + this, "newThemeMode", newUiMode, "currentUiMode", currentUiMode);
        if (ScreenTypeUtils.getInstance().isScreenChange(newConfig)) {
            Logger.i(LIFE_CYCLE_TAG, "onConfigurationChanged " + this, "分屏 不执行操作", newConfig.screenWidthDp);
            if (currentUiMode != newUiMode) {
                currentUiMode = newUiMode;
                onApplySkin(ThemeUtils.INSTANCE.getCurrentTheme(getApplicationContext()), true);
            }
            return;
        }
        if (currentUiMode == newUiMode) {
            Logger.i(LIFE_CYCLE_TAG, "onConfigurationChanged " + this, "主题样式不变");
        } else {
            ThemeUtils.INSTANCE.setCurrentUiMode(newUiMode);
            currentUiMode = newUiMode;
            recreateFrom = ThemeUtils.INSTANCE.isNightModeEnabled(getApplicationContext())
                    ? ThemeType.getThemeValueByType(ThemeType.NIGHT) : ThemeType.getThemeValueByType(ThemeType.DAY);
            Logger.i(LIFE_CYCLE_TAG, "onConfigurationChanged " + this, "recreateFrom", recreateFrom);
            FragmentIntent.closeAllFragment(getSupportFragmentManager());
            recreate();
        }
    }

    @Override
    public void addFragment(final BaseFragment fragment, final Bundle bundle) {
        FragmentIntent.addFragment(mScreenId, onFragmentId(), getSupportFragmentManager(),
                fragment, bundle);
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            onMoveMapCenter(bundle);
        }
        onFragmentSizeChanged();
    }

    @Override
    public void addFragment(final BaseFragment fragment, final Bundle bundle, boolean isHideCurFragment) {
        FragmentIntent.addFragment(mScreenId, onFragmentId(), getSupportFragmentManager(),
                fragment, bundle, isHideCurFragment);
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            onMoveMapCenter(bundle);
        }
        onFragmentSizeChanged();
    }

    @Override
    public void addPoiDetailsFragment(BaseFragment fragment, Bundle bundle) {
        FragmentIntent.addPoiDetailsFragment(mScreenId, onFragmentId(), getSupportFragmentManager(),
                fragment, bundle);
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            onMoveMapCenter();
        }
        onFragmentSizeChanged();
    }

    @Override
    public void refreshFragment() {
        int fragmentStackSize = mStackManager.getFragmentSize(mScreenId);
        if (0 == fragmentStackSize) {
            Logger.i(LIFE_CYCLE_TAG, "InstanceState " + this, "新建不需要恢复任何数据");
        } else {
            Logger.i(LIFE_CYCLE_TAG, "InstanceState " + this, "有数据可恢复", this.getTaskId()
                    , "current_stack_fragment", fragmentStackSize
                    , "current_stack_activity", mStackManager.getActivitySize(mScreenId));
            BaseFragment fragment = mStackManager.popFragment(mScreenId);
            mStackManager.removeAllFragment(mScreenId);
            if (null != fragment) {
                Bundle bundle = fragment.getArguments();
                FragmentIntent.restoreFragment(mScreenId, onFragmentId(), getSupportFragmentManager(), fragment, bundle);
                onMoveMapCenter(bundle);
                onFragmentSizeChanged();
            } else {
                Logger.i(LIFE_CYCLE_TAG, "InstanceState" + this, "栈集合为空没有数据可恢复");
            }
        }
    }

    @Override
    public void restoreFragments() {
        int fragmentStackSize = mStackManager.getFragmentSize(mScreenId);
        if (0 == fragmentStackSize) {
            Logger.i(LIFE_CYCLE_TAG, "InstanceState " + this, "新建不需要恢复任何数据");
        } else {
            Logger.i(LIFE_CYCLE_TAG, "InstanceState " + this, "有数据可恢复", this.getTaskId()
                    , "current_stack_fragment", fragmentStackSize
                    , "current_stack_activity", mStackManager.getActivitySize(mScreenId));

            FragmentIntent.restoreFragments(mScreenId, onFragmentId(), getSupportFragmentManager());
            onMoveMapCenter();
            onFragmentSizeChanged();
        }
    }

    @Override
    public void closeFragment(final boolean nextShow) {
        BaseFragment fragment = FragmentIntent.closeFragment(mScreenId, getSupportFragmentManager(), nextShow);
        Bundle bundle = null;
        if (fragment != null && fragment.getClass().getName().contains("NaviGuidanceFragment")) {
            bundle = new Bundle();
            bundle.putInt("bundle_key_route_start_navi_sim", 0);
        }
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            if (bundle != null) {
                onMoveMapCenter(bundle);
                return;
            }
            onMoveMapCenter();
        }
        onFragmentSizeChanged();
    }

    @Override
    public void closeFragment(Bundle bundle) {
        BaseFragment fragment = FragmentIntent.closeFragment(mScreenId, getSupportFragmentManager(),
                bundle);
        Bundle newBundle = null;
        if (fragment != null && fragment.getClass().getName().contains("NaviGuidanceFragment")) {
            newBundle = new Bundle();
            newBundle.putInt("bundle_key_route_start_navi_sim", 0);
        }
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            if (newBundle != null) {
                onMoveMapCenter(newBundle);
                return;
            }
            onMoveMapCenter();
        }
        onFragmentSizeChanged();
    }

    @Override
    public void closeAllFragment() {
        FragmentIntent.closeAllFragment(mScreenId, getSupportFragmentManager());
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            onMoveMapCenter();
        }
        onFragmentSizeChanged();
    }

    @Override
    public void closeAllFragmentUpRoute() {
        FragmentIntent.closeAllFragmentUpRoute(mScreenId, getSupportFragmentManager());
        onFragmentSizeChanged();
    }

    @Override
    public void closeAllFragmentUpRoute(String className) {
        FragmentIntent.closeIndexBeforeFragment(mScreenId, getSupportFragmentManager(), className);
        onFragmentSizeChanged();
    }

    @Override
    public void closeAllFragmentUpNavi() {
        Fragment fragment = FragmentIntent.closeAllFragmentUpNavi(
                mScreenId, getSupportFragmentManager());
        Bundle newBundle = null;
        if (null != fragment && fragment.getClass().getName().contains("NaviGuidanceFragment")) {
            newBundle = new Bundle();
            newBundle.putInt("bundle_key_route_start_navi_sim", 0);
        }
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            if (newBundle != null) {
                onMoveMapCenter(newBundle);
                return;
            }
            onMoveMapCenter();
        }
        onFragmentSizeChanged();
    }

    @Override
    public void closeAllFragmentsUntilTargetFragment(final String targetClassName) {
        FragmentIntent.closeAllFragmentsUntilTargetFragment(mScreenId, getSupportFragmentManager(), targetClassName);
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            onMoveMapCenter();
        }
        onFragmentSizeChanged();
    }

    @Override
    public void closeAllFragmentAndSearchView() {
        FragmentIntent.closeAllFragment(mScreenId, getSupportFragmentManager());
        onMoveMapCenter();
        onFragmentSizeChanged();
    }

    @Override
    public void showCurrentFragment() {
        FragmentIntent.showCurrentFragment(mScreenId, getSupportFragmentManager());
    }

    protected VM initViewModel() {
        return null;
    }

    /**
     * 创建ViewModel
     */
    private void createViewModel() {
        mViewModel = initViewModel();
        if (mViewModel == null) {
            final Class modelClass;
            final Type type = getClass().getGenericSuperclass();
            if (type instanceof ParameterizedType) {
                // 获取直接继承的父类(也就是BaseActivity本身)的第二个泛型参数Class
                modelClass = (Class) ((ParameterizedType) type).getActualTypeArguments()[1];
            } else {
                // If no generic parameters are specified, BaseViewModel is used by default.
                modelClass = BaseViewModel.class;
            }
            mViewModel = (VM) createViewModel(modelClass);
        }
    }

    private void bindContentView() {
        if (ConvertUtils.isEmpty(mViewModel)) {
            return;
        }

        final int mViewModelId = onInitVariableId();
        mBinding = DataBindingUtil.setContentView(this, onLayoutId());
        mBinding.setVariable(mViewModelId, mViewModel);
        mBinding.setLifecycleOwner(this);
        getLifecycle().addObserver(mViewModel);
        mViewModel.attachView(this, mScreenId);
    }

    /**
     * 创建ViewModel
     *
     * @param cls ViewModel类
     * @param <T> AndroidViewModel
     * @return AndroidViewModel
     */

    private <T extends AndroidViewModel> T createViewModel(final Class<T> cls) {
        return new ViewModelProvider(this).get(cls);
    }

    protected void onMoveMapCenter() {

    }

    protected void onMoveMapCenter(final Bundle bundle) {

    }

    protected void onResetMapCenter() {

    }

    /***
     * fragment 容器发生变化的时候触发
     */
    protected void onFragmentSizeChanged() {

    }


    /**
     * 设置沉浸式状态栏
     */
    private void setImmersiveStatusBar() {
        final Window window = getWindow();
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            // 清除之前的标志，确保没有残留影响
            window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
            // 使内容布局延伸到状态栏下面
            window.getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN |
                    View.SYSTEM_UI_FLAG_LAYOUT_STABLE);
            // 允许设置状态栏的背景颜色
            window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
            // 设置状态栏的颜色
            window.setStatusBarColor(0);
        } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
            // 对于 Android 4.4 到 5.0 的系统，使用透明状态栏
            window.addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
            // 调整布局，使内容布局延伸到状态栏下
            final View decorView = window.getDecorView();
            final int option = View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN | View.SYSTEM_UI_FLAG_LAYOUT_STABLE;
            decorView.setSystemUiVisibility(option);
        }
    }


    public String getLastClosedFragmentName() {
        return mLastClosedFragmentName;
    }

    public void setMLastClosedFragmentName(final String lastClosedFragmentName) {
        this.mLastClosedFragmentName = lastClosedFragmentName;
    }

}