package com.sgm.navi.ui.base;

import android.content.Context;
import android.graphics.Rect;
import android.os.Build;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.view.ViewTreeObserver;
import android.view.Window;
import android.view.WindowManager;
import android.view.inputmethod.InputMethodManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.databinding.DataBindingUtil;
import androidx.databinding.ViewDataBinding;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.ViewModelProvider;

import com.android.utils.ConvertUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

public abstract class BaseActivity<V extends ViewDataBinding, VM extends BaseViewModel>
        extends AppCompatActivity implements IBaseView {
    protected V mBinding;
    protected VM mViewModel;
    protected StackManager mStackManager;
    protected String mScreenId;
    private String mLastClosedFragmentName;
    private final String LIFE_CYCLE_TAG = "activity_life_cycle";
    public boolean isMapActivity = false;

    public BaseActivity() {
        super();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onCreate before");
        mStackManager = StackManager.getInstance();
        onCreateBefore();
        mStackManager.push(mScreenId, this);
    }

    @Override
    protected void onCreate(final @Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onCreate start");
        setImmersiveStatusBar();
        createViewModel();
        boolean isBindView = onCreateViewBefore();
        if (!isBindView) return;
        bindContentView();
        onInitView();
        onInitObserver();
        onInitData();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onCreate end");
    }

    @Override
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        Logger.i(LIFE_CYCLE_TAG, "onSaveInstanceState");
        outState.putBoolean("onConfigurationChanged", true);
    }

    @Override
    protected void onRestoreInstanceState(@NonNull Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
        Logger.i(LIFE_CYCLE_TAG, "onRestoreInstanceState", savedInstanceState.getBoolean("onConfigurationChanged"));

    }

    @Override
    protected void onResume() {
        super.onResume();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onResume");
    }

    @Override
    protected void onStart() {
        super.onStart();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onStart");
    }

    @Override
    protected void onPause() {
        super.onPause();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onPause");
    }

    @Override
    protected void onStop() {
        super.onStop();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onStop");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "onDestroy");
        mStackManager.removeBaseView(mScreenId, this);
        ToastUtils.Companion.getInstance().cancelView();
    }

    @Override
    public void finish() {
        super.finish();
        Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "finish");
        mStackManager.popActivity(mScreenId);
    }

    protected boolean isBackPressed() {
        return false;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (!isBackPressed()) return true;
        hideInput();
        if (KeyEvent.KEYCODE_BACK == keyCode) {
            int fragmentSize = mStackManager.getFragmentSize(mScreenId);
            Logger.i(LIFE_CYCLE_TAG, getClass().getSimpleName(), "物理返回键拦截", "fragmentSize", fragmentSize);
            if (fragmentSize > 0) {
                // closeFragment(true);// 无法直接closFragment,因为无法保证地图状态统一
                BaseFragment currentFragment = mStackManager.getCurrentFragment(mScreenId);
                if (!ConvertUtils.isEmpty(currentFragment)) {
                    currentFragment.onBackPressed();
                    return true;
                }
            }
        }
        return super.onKeyDown(keyCode, event);
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
        onFragmentSizeChanged(fragment.getClass().getSimpleName().equals("SplitFragment"));
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
        onFragmentSizeChanged(false);
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
        onFragmentSizeChanged(false);
    }

    @Override
    public void closeFragment(final boolean nextShow) {
        BaseFragment currentFragment = mStackManager.getCurrentFragment(mScreenId);
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
        onFragmentSizeChanged(currentFragment != null && fragment.getClass().getSimpleName().equals("SplitFragment"));
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
        onFragmentSizeChanged(false);
    }

    @Override
    public void closeFragmentFromDetail(final boolean nextShow) {
        FragmentIntent.closeFragment(mScreenId, getSupportFragmentManager(), nextShow);
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapTabFromDetail();
        }
        onFragmentSizeChanged(false);
    }

    @Override
    public void closeTrafficEventFragment(boolean nextShow) {
        FragmentIntent.closeFragment(mScreenId, getSupportFragmentManager(), nextShow);
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapTabFromDetail();
        }
        onFragmentSizeChanged(false);
    }

    @Override
    public void closeAllFragment() {
        FragmentIntent.closeAllFragment(mScreenId, getSupportFragmentManager());
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            onMoveMapCenter();
        }
        onFragmentSizeChanged(false);
    }

    @Override
    public void closeAllFragmentUpRoute() {
        FragmentIntent.closeAllFragmentUpRoute(mScreenId, getSupportFragmentManager());
        onFragmentSizeChanged(false);
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
        onFragmentSizeChanged(false);
    }

    @Override
    public void closeAllFragmentsUntilTargetFragment(final String targetClassName) {
        FragmentIntent.closeAllFragmentsUntilTargetFragment(mScreenId, getSupportFragmentManager(), targetClassName);
        if (mStackManager.isFragmentStackNull(mScreenId)) {
            onResetMapCenter();
        } else {
            onMoveMapCenter();
        }
        onFragmentSizeChanged(false);
    }

    @Override
    public void closeAllFragmentAndSearchView() {
        FragmentIntent.closeAllFragment(mScreenId, getSupportFragmentManager());
        onMoveMapCenter();
        onFragmentSizeChanged(false);
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

    protected void onResetMapTabFromDetail() {

    };

    /***
     * fragment 容器发生变化的时候触发
     */
    protected void onFragmentSizeChanged(boolean isSpiltFragment) {

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

    public void hideInput() {
        final InputMethodManager imm = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
        Window window = getWindow();
        if (imm != null && null != window) {
            imm.hideSoftInputFromWindow(window.getDecorView().getWindowToken(), 0);
        }
    }
}