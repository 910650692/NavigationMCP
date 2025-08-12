package com.sgm.navi.ui.base;

import android.annotation.SuppressLint;
import android.os.Bundle;

import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

import java.util.List;


@SuppressLint("CommitTransaction")
public class FragmentIntent {
    private static final String TAG = FragmentIntent.class.getSimpleName();
    private static final StackManager STACKMANAGER = StackManager.getInstance();

    /**
     * 添加Fragment
     *
     * @param screenId        屏幕ID
     * @param containerId     容器ID
     * @param fragmentManager fragmentManager
     * @param toFragment      BaseFragment
     * @param bundle          参数
     */
    public static void addFragment(final String screenId, final int containerId,
                                   final FragmentManager fragmentManager,
                                   final BaseFragment toFragment,
                                   final Bundle bundle) {
        addFragment(screenId, containerId, fragmentManager, toFragment, bundle, true);
    }

    /**
     * 添加Fragment
     *
     * @param screenId          屏幕ID
     * @param containerId       容器ID
     * @param fragmentManager   fragmentManager
     * @param toFragment        BaseFragment
     * @param bundle            参数
     * @param isHideCurFragment 是否隐藏当前fragment
     */
    public static void addFragment(final String screenId, final int containerId,
                                   final FragmentManager fragmentManager,
                                   final BaseFragment toFragment,
                                   final Bundle bundle,
                                   final boolean isHideCurFragment) {
        if (ConvertUtils.isEmpty(toFragment)) {
            return;
        }
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        if (STACKMANAGER.isFragmentStackNull(screenId)) {
            Logger.i(TAG, "current fragment stack is null", screenId, toFragment.getClass().getName());
            toFragment.setArguments(bundle);
            transaction.add(containerId, toFragment);
            transaction.show(toFragment);
            STACKMANAGER.push(screenId, toFragment);
            transaction.commitAllowingStateLoss();
            return;
        }
        final BaseFragment currentFragment = STACKMANAGER.getCurrentFragment(screenId);
        if (ConvertUtils.equals(currentFragment, toFragment)) {
            Logger.i(TAG, "current fragment == target fragment");
            toFragment.onNewIntent(bundle);
            return;
        }
        if (STACKMANAGER.isContain(screenId, toFragment)) {
            Logger.i(TAG, "contain target fragment isHideCurFragment:", isHideCurFragment, " isAdded:",
                    currentFragment.isAdded(), " currentFragment:", currentFragment.getClass().getSimpleName()
                    , " toFragment:", toFragment.getClass().getSimpleName());
            STACKMANAGER.removeBaseView(screenId, toFragment);
            STACKMANAGER.push(screenId, toFragment);
            if (isHideCurFragment) {
                if (currentFragment.isAdded()) {
                    if(currentFragment.getParentFragmentManager() == fragmentManager){
                        Logger.d(TAG, "transaction.hide ", currentFragment.getClass().getSimpleName());
                        transaction.hide(currentFragment);
                    }else{
                        if(!ConvertUtils.isNull(currentFragment.getParentFragmentManager())){
                            Logger.d(TAG, "fragmentManager is error hide ", currentFragment.getClass().getSimpleName());
                            currentFragment.getParentFragmentManager().beginTransaction().hide(currentFragment);
                        } else {
                            Logger.d(TAG, "currentFragment.getParentFragmentManager() is null ", currentFragment.getClass().getSimpleName());
                        }
                    }
                }
            }
            transaction.show(toFragment);
            toFragment.onNewIntent(bundle);
        } else {
            Logger.i(TAG, "no contain target fragment isHideCurFragment:", isHideCurFragment, " isAdded:",
                    currentFragment.isAdded(), " currentFragment:", currentFragment.getClass().getSimpleName(),
                    " toFragment:", toFragment.getClass().getSimpleName());
            toFragment.setArguments(bundle);
            if (isHideCurFragment) {
                if (currentFragment.isAdded()) {
                    if(currentFragment.getParentFragmentManager() == fragmentManager){
                        Logger.d(TAG, "transaction.hide ", currentFragment.getClass().getSimpleName());
                        transaction.hide(currentFragment);
                    }else{
                        if(!ConvertUtils.isNull(currentFragment.getParentFragmentManager())){
                            Logger.d(TAG, "fragmentManager is error hide ", currentFragment.getClass().getSimpleName());
                            currentFragment.getParentFragmentManager().beginTransaction().hide(currentFragment);
                        } else {
                            Logger.d(TAG, "currentFragment.getParentFragmentManager() is null ", currentFragment.getClass().getSimpleName());
                        }
                    }
                }
            }
            transaction.add(containerId, toFragment);
            transaction.show(toFragment);
            STACKMANAGER.push(screenId, toFragment);
        }
        transaction.commitAllowingStateLoss();
    }

    /**
     * 添加 PoiDetailsFragment
     *
     * @param screenId        屏幕ID
     * @param containerId     容器ID
     * @param fragmentManager fragmentManager
     * @param toFragment      BaseFragment
     * @param bundle          参数
     */
    public static void addPoiDetailsFragment(final String screenId, final int containerId,
                                             final FragmentManager fragmentManager,
                                             final BaseFragment toFragment,
                                             final Bundle bundle) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        try {
            // 查找是否已经存在相同类型的 PoiDetailsFragment
            Fragment existingFragment = null;
            for (Fragment fragment : fragmentManager.getFragments()) {
                if (fragment.getClass().getName().equals(toFragment.getClass().getName())) {
                    existingFragment = fragment;
                    break;
                }
            }
            if (existingFragment == null && !ConvertUtils.isNull(StackManager.getInstance().getBaseFragmentStack(screenId))) {
                for (Fragment fragment : StackManager.getInstance().getBaseFragmentStack(screenId)) {
                    if (fragment.getClass().getName().equals(toFragment.getClass().getName())) {
                        existingFragment = fragment;
                        break;
                    }
                }
            }
            // 如果已存在相同类型的 Fragment，则移除
            if (existingFragment != null) {
                if (Logger.openLog) {
                    Logger.i(TAG, toFragment.getClass().getSimpleName(), " 已存在，先移除");
                }
                transaction.remove(existingFragment);
                STACKMANAGER.removeBaseView(screenId, (IBaseView) existingFragment);
            }
            if (!STACKMANAGER.isFragmentStackNull(screenId)) {
                int isHide = 0;
                if (!ConvertUtils.isEmpty(bundle)) {
                    isHide = bundle.getInt("no_hide_fragment", 0);
                }
                //如果查找是否存在栈顶的其他Fragment，如果有则隐藏
                final BaseFragment currentFragment = STACKMANAGER.getCurrentFragment(screenId);
                if (currentFragment.isAdded() && isHide == 0){
                    if(currentFragment.getParentFragmentManager() == fragmentManager){
                        Logger.d(TAG, "transaction.hide ", currentFragment.getClass().getSimpleName());
                        transaction.hide(currentFragment);
                    } else {
                        if(!ConvertUtils.isNull(currentFragment.getParentFragmentManager())){
                            Logger.d(TAG, "fragmentManager is error hide ", currentFragment.getClass().getSimpleName());
                            currentFragment.getParentFragmentManager().beginTransaction().hide(currentFragment);
                        } else {
                            Logger.d(TAG, "currentFragment.getParentFragmentManager() is null");
                        }
                    }
                } else {
                    if (Logger.openLog) {
                        Logger.i(TAG, "当前Fragment不需要隐藏", currentFragment.getClass().getSimpleName());
                    }
                }
            }

            if (Logger.openLog) {
                Logger.i(TAG, "添加新的 ", toFragment.getClass().getSimpleName());
            }
            toFragment.setArguments(bundle);
            transaction.add(containerId, toFragment);
            transaction.show(toFragment);
            STACKMANAGER.push(screenId, toFragment);
        } catch (Exception e) {
            Logger.e(TAG, "Error in addPoiDetailsFragment: ", e);
        } finally {
            transaction.commitAllowingStateLoss();
        }
    }


    /**
     * 添加Fragment
     *
     * @param screenId        屏幕ID
     * @param fragmentManager fragmentManager
     * @param nextShow        参数
     */
    public static BaseFragment closeFragment(final String screenId, final FragmentManager fragmentManager, final boolean nextShow) {
        BaseFragment currentFragment = null;
        //syncFragmentList(screenId, fragmentManager);
        try {
            currentFragment = STACKMANAGER.popFragment(screenId);
            final FragmentTransaction transaction = fragmentManager.beginTransaction();
            if (!ConvertUtils.isEmpty(currentFragment)) {
                if (Logger.openLog) {
                    Logger.i(TAG, "remove:", currentFragment.getClass().getSimpleName());
                }
                transaction.remove(currentFragment);
            } else {
                if (Logger.openLog) {
                    Logger.i(TAG, "对象为空无法移除", currentFragment);
                }
            }
            if (nextShow) {
                final BaseFragment toFragment = STACKMANAGER.getCurrentFragment(screenId);
                if (Logger.openLog && toFragment != null) {
                    Logger.i(TAG, "移除上一个。显示下一个", toFragment.getClass().getSimpleName());
                }
                if (!ConvertUtils.isEmpty(toFragment)) {
                    if (toFragment.getClass().getName().contains("NaviGuidanceFragment")) {
                        Bundle bundle = new Bundle();
                        bundle.putInt("NAVI_CONTROL", 1);
                        toFragment.onNewIntent(bundle);
                    }
                    currentFragment = toFragment;
                    FragmentManager fragmentManager1 = currentFragment.getParentFragmentManager();
                    if (fragmentManager1 == fragmentManager) {
                        Logger.d(TAG, "transaction.show ", toFragment.getClass().getSimpleName());
                        transaction.show(toFragment);
                    }else{
                        if(!ConvertUtils.isNull(fragmentManager1)){
                            Logger.d(TAG, "fragmentManager is error show ", toFragment.getClass().getSimpleName());
                            fragmentManager1.beginTransaction().show(toFragment);
                        } else {
                            Logger.d(TAG, "error currentFragment.getParentFragmentManager() is null");
                        }
                    }
                } else {
                    Logger.i(TAG, "回退栈为空，已经没有可显示的Fragment");
                }
            }
            transaction.commitAllowingStateLoss();
        } catch (Exception e) {
            Logger.e(TAG, "Error in closeFragment: ", e.getMessage(), e);
        }
        return currentFragment;
    }

    public static BaseFragment closeFragment(final String screenId,
                                             final FragmentManager fragmentManager,
                                             final Bundle bundle) {
        BaseFragment currentFragment;
        currentFragment = STACKMANAGER.popFragment(screenId);
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        if (!ConvertUtils.isEmpty(currentFragment)) {
            if (Logger.openLog) {
                Logger.i(TAG, "remove:", currentFragment.getClass().getSimpleName());
            }
            transaction.remove(currentFragment);
        } else {
            if (Logger.openLog) {
                Logger.i(TAG, "对象为空无法移除", currentFragment);
            }
        }
        final BaseFragment toFragment = STACKMANAGER.getCurrentFragment(screenId);
        if (Logger.openLog && toFragment != null) {
            Logger.i(TAG, "移除上一个。显示下一个", toFragment.getClass().getSimpleName());
        }
        if (!ConvertUtils.isEmpty(toFragment)) {
            currentFragment = toFragment;
            toFragment.onNewIntent(bundle);
            if (currentFragment.getParentFragmentManager() == fragmentManager) {
                Logger.d(TAG, "transaction.show ", toFragment.getClass().getSimpleName());
                transaction.show(toFragment);
            } else {
                if(!ConvertUtils.isNull(currentFragment.getParentFragmentManager())){
                    Logger.d(TAG, "fragmentManager is error show:", toFragment.getClass().getSimpleName());
                    currentFragment.getParentFragmentManager().beginTransaction().show(toFragment);
                } else {
                    Logger.d(TAG, "error currentFragment.getParentFragmentManager() is null");
                }
            }
        } else {
            Logger.i(TAG, "回退栈为空，已经没有可显示的Fragment");
        }
        transaction.commitAllowingStateLoss();
        return currentFragment;
    }

    /**
     * 显示当前被hide的fragment
     *
     * @param screenId        屏幕ID
     * @param fragmentManager fragmentManager
     */
    public static void showCurrentFragment(final String screenId, final FragmentManager fragmentManager) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        final BaseFragment toFragment = STACKMANAGER.getCurrentFragment(screenId);
        if (Logger.openLog && toFragment != null) {
            Logger.i(TAG, "showCurrentFragment", toFragment.getClass().getSimpleName());
        }
        if (!ConvertUtils.isEmpty(toFragment)) {
            if (toFragment.getParentFragmentManager() == fragmentManager) {
                if (Logger.openLog) {
                    Logger.i(TAG, "show:", toFragment.getClass().getSimpleName(), " isAdded:",
                            toFragment.isAdded(), " ,isHidden:", toFragment.isHidden());
                }
                transaction.show(toFragment);
            }else{
                if(!ConvertUtils.isNull(toFragment.getParentFragmentManager())){
                    Logger.d(TAG, "fragmentManager is error show:", toFragment.getClass().getSimpleName());
                    toFragment.getParentFragmentManager().beginTransaction().show(toFragment);
                } else {
                    Logger.d(TAG, "error toFragment.getParentFragmentManager() is null");
                }
            }
        } else {
            Logger.i(TAG, "当前Fragment为空，无法显示");
        }
        final List<Fragment> fragments = fragmentManager.getFragments();
        if (!fragments.isEmpty()) {
            for (int t = 0; t < fragments.size(); t++) {
                if (Logger.openLog) {
                    Logger.i(TAG, "showCurrentFragment:", fragments.get(t).getClass().getSimpleName()
                            , " isAdded:", fragments.get(t).isAdded(), " isHidden:", fragments.get(t).isHidden());
                }
            }
        }
        transaction.commit();
    }

    /**
     * 关闭所有Fragment
     *
     * @param screenId        屏幕ID
     * @param fragmentManager fragmentManager
     */
    public static void closeAllFragmentUpRoute(final String screenId, final FragmentManager fragmentManager) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        final List<Fragment> fragments = fragmentManager.getFragments();
        int index = -1;
        if (!fragments.isEmpty()) {
            for (int t = 0; t < fragments.size(); t++) {
                if (fragments.get(t).getClass().getName().contains("RouteFragment")
                        || fragments.get(t).getClass().getName().contains("NaviGuidanceFragment")) {
                    index = t;
                    if (Logger.openLog) {
                        Logger.i(TAG, "remove top index: ", t);
                    }
                }
                if (index < t && index != -1) {
                    if (Logger.openLog) {
                        Logger.i(TAG, "transaction remove top fragment: ", fragments.get(t).getClass().getName());
                    }
                    transaction.remove(fragments.get(t));
                    if (!fragments.get(t).getClass().getName().contains("SupportRequestManagerFragment")) {
                        if (Logger.openLog) {
                            Logger.i(TAG, "mStackManager remove top fragment: ", fragments.get(t).getClass().getName());
                        }
                        STACKMANAGER.removeBaseView(screenId, (IBaseView) fragments.get(t));
                    }
                }
            }
        }
        if (index != -1) {
            Fragment fragment = fragments.get(index);
            if (fragment.getClass().getName().contains("NaviGuidanceFragment")) {
                Bundle bundle = new Bundle();
                bundle.putInt("NAVI_CONTROL", 1);
                if (fragment instanceof BaseFragment<?, ?>) {
                    BaseFragment baseFragment = (BaseFragment) fragment;
                    baseFragment.onNewIntent(bundle);
                }
            }
            if (Logger.openLog) {
                Logger.i(TAG, "transaction show top fragment: ", fragment.getClass().getName());
            }
            transaction.show(fragment);
        }
        transaction.commitAllowingStateLoss();
    }

    /**
     * 关闭导航Fragment之上的所有Fragment
     *
     * @param screenId        屏幕ID
     * @param fragmentManager FragmentManager
     */
    public static Fragment closeAllFragmentUpNavi(final String screenId,
                                                  final FragmentManager fragmentManager) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        final List<Fragment> fragments = fragmentManager.getFragments();
        int index = -1;
        if (!fragments.isEmpty()) {
            for (int t = 0; t < fragments.size(); t++) {
                if (fragments.get(t).getClass().getName().contains("NaviGuidanceFragment")) {
                    index = t;
                    if (Logger.openLog) {
                        Logger.i(TAG, "remove top index: ", t);
                    }
                }
                if (index < t && index != -1) {
                    if (Logger.openLog) {
                        Logger.i(TAG, "transaction remove top fragment: ",
                                fragments.get(t).getClass().getName());
                    }
                    transaction.remove(fragments.get(t));
                    if (!fragments.get(t).getClass().getName().
                            contains("SupportRequestManagerFragment")) {
                        if (Logger.openLog) {
                            Logger.i(TAG, "mStackManager remove top fragment: ",
                                    fragments.get(t).getClass().getName());
                        }
                        STACKMANAGER.removeBaseView(screenId, (IBaseView) fragments.get(t));
                    }
                }
            }
        }
        Fragment fragment = null;
        if (index != -1) {
            fragment = fragments.get(index);
            if (fragment.getClass().getName().contains("NaviGuidanceFragment")) {
                Bundle bundle = new Bundle();
                bundle.putInt("NAVI_CONTROL", 1);
                if (fragment instanceof BaseFragment<?, ?>) {
                    BaseFragment baseFragment = (BaseFragment) fragment;
                    baseFragment.onNewIntent(bundle);
                }
            }
            if (Logger.openLog) {
                Logger.i(TAG, "transaction show fragment: ", fragment.getClass().getName());
            }
            transaction.show(fragment);
        }
        transaction.commitAllowingStateLoss();
        return fragment;
    }


    /**
     * 关闭所有Fragment
     *
     * @param screenId        屏幕ID
     * @param fragmentManager fragmentManager
     * @param className       目标fragment的类名
     */
    public static void closeAllFragmentsUntilTargetFragment(final String screenId,
                                                            final FragmentManager fragmentManager,
                                                            final String className) {
        if (Logger.openLog) {
            Logger.i(TAG, "closeAllFragmentsUntilTargetFragment className: ", className);
        }
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        final List<Fragment> fragments = fragmentManager.getFragments();
        int index = -1;
        if (!fragments.isEmpty()) {
            for (int t = 0; t < fragments.size(); t++) {
                if (fragments.get(t).getClass().getName().contains(className)) {
                    index = t;
                    if (Logger.openLog) {
                        Logger.i(TAG, "remove top index: ", t);
                    }
                }
                if (index <= t && index != -1) {
                    if (Logger.openLog) {
                        Logger.i(TAG, "remove top fragment: ", fragments.get(t).getClass().getName());
                    }
                    transaction.remove(fragments.get(t));
                    STACKMANAGER.removeBaseView(screenId, (IBaseView) fragments.get(t));
                }
            }
        }
        transaction.commitAllowingStateLoss();
    }

    /**
     * 关闭所有Fragment
     *
     * @param screenId        屏幕ID
     * @param fragmentManager FragmentManager
     */
    public static void closeAllFragment(final String screenId, final FragmentManager fragmentManager) {
        final FragmentTransaction transaction = fragmentManager.beginTransaction();
        for (Fragment fragment : fragmentManager.getFragments()) {
            if (Logger.openLog) {
                Logger.i(TAG, "screenId:", screenId, " remove fragment: ", fragment.getClass().getName());
            }
            transaction.remove(fragment);
        }
        transaction.commitAllowingStateLoss();
        // TODO: 2025/4/6 @xuqun 检查下我这么修改影响你的逻辑不 这么修改是为了换肤后栈集合清理不彻底的问题
        STACKMANAGER.removeAllFragment(screenId);
    }

    /**
     * 关闭所有Fragment
     */
    public void closeAllFragment() {
        Logger.i(TAG, "closeAllFragment");
        STACKMANAGER.removeAllFragment();
    }

    /**
     * 同步默认栈集合和原生栈集合，应对换肤场景
     *
     * @param screenId        屏幕Id
     * @param fragmentManager 碎片管理器
     */
    public static void syncFragmentList(final String screenId, final FragmentManager fragmentManager) {
        List<Fragment> fragments = fragmentManager.getFragments();
        STACKMANAGER.removeAllFragment(screenId);
        if (!ConvertUtils.isEmpty(fragments)) {
            for (Fragment fragment : fragments) {
                if (fragment instanceof IBaseView baseFragment) {
                    STACKMANAGER.push(screenId, baseFragment);
                }
            }
            Logger.d(TAG, "Stack Fragment Manager", STACKMANAGER.getFragmentSize(screenId));
        } else {
            Logger.d(TAG, "fragmentManager管理栈为空");
        }
    }
}
