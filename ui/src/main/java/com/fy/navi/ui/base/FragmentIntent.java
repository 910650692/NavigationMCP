package com.fy.navi.ui.base;

import android.annotation.SuppressLint;
import android.os.Bundle;

import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/22
 */
@SuppressLint("CommitTransaction")
public class FragmentIntent {
    private static final String TAG = FragmentIntent.class.getSimpleName();
    private static int FRAGMENT_TAG = 0;
    public static final String DEFAULT_TAG_KEY = String.valueOf(FRAGMENT_TAG++);
    public static final int FRAGMENT_MODE_STANDARD = 0x001;
    public static final int FRAGMENT_MODE_TOP = 0x002;
    public static final int FRAGMENT_MODE_TASK = 0x003;
    public static final int FRAGMENT_MODE_SINGLETASK = 0x004;
    private static final StackManager mStackManager = StackManager.getInstance();

    public void addFragment(String screenId, int containerId, FragmentManager fragmentManager, Class<? extends BaseFragment> clazz, int launchMode, Bundle bundle) {
        switch (launchMode) {
            case FRAGMENT_MODE_STANDARD:
                addStandardFragment(screenId, containerId, fragmentManager, clazz, bundle);
                break;
            case FRAGMENT_MODE_TOP:
                break;
            case FRAGMENT_MODE_TASK:
                break;
            case FRAGMENT_MODE_SINGLETASK:
                break;
            default:
                Logger.i(TAG, "please setup fragment launchMode");
                break;
        }
    }

    private void addStandardFragment(String screenId, int containerId, FragmentManager fragmentManager, Class<? extends BaseFragment> clazz, Bundle bundle) {
        try {
//            String tag = ConvertUtils.isEmpty(bundle.getString("DEFAULT_TAG_KEY")) ? DEFAULT_TAG_KEY : bundle.getString("DEFAULT_TAG_KEY");
            Constructor<? extends BaseFragment> constructor = clazz.getConstructor();
            BaseFragment fragment = constructor.newInstance();
            if (!ConvertUtils.isEmpty(bundle)) fragment.setArguments(bundle);
            FragmentTransaction transaction = fragmentManager.beginTransaction();
            BaseFragment fromFragment = mStackManager.getCurrentFragment(screenId);
            if (ConvertUtils.isEmpty(fromFragment)) return;
            transaction.hide(fromFragment);
            transaction.add(containerId, fragment).addToBackStack(fragment.getClass().getName());
            transaction.commitAllowingStateLoss();
            mStackManager.push(screenId, fragment);
        } catch (IllegalAccessException | InstantiationException | NoSuchMethodException |
                 InvocationTargetException exception) {
            Logger.i(TAG, exception.toString());
        }
    }

    private void addSingleTopFragment(String screenId, int containerId, FragmentManager fragmentManager, Class<? extends BaseFragment> clazz, Bundle bundle) {
        try {
            String tag = ConvertUtils.isEmpty(bundle.getString("DEFAULT_TAG_KEY")) ? DEFAULT_TAG_KEY : bundle.getString("DEFAULT_TAG_KEY");
            BaseFragment currentFragment = mStackManager.getCurrentFragment(screenId);
            BaseFragment baseFragment = (BaseFragment) fragmentManager.findFragmentByTag(tag);
            if (ConvertUtils.equals(currentFragment, baseFragment)) {
                baseFragment.onNewIntent(bundle);
            } else {
                Constructor<? extends BaseFragment> constructor = clazz.getConstructor();
                BaseFragment fragment = constructor.newInstance();
                FragmentTransaction transaction = fragmentManager.beginTransaction();
                transaction.add(containerId, fragment).addToBackStack(fragment.getClass().getName());
                transaction.commitAllowingStateLoss();
                mStackManager.push(screenId, fragment);
            }
        } catch (Exception exception) {
            Logger.i(TAG, exception.toString());
        }
    }

    private void addSingleTaskFragment(String screenId, int containerId, FragmentManager fragmentManager, Class<? extends BaseFragment> clazz, Bundle bundle) {

    }

    public static void addFragment(String screenId, int containerId, FragmentManager fragmentManager, BaseFragment toFragment, Bundle bundle) {
        if (ConvertUtils.isEmpty(toFragment)) return;
        FragmentTransaction transaction = fragmentManager.beginTransaction();
        if (mStackManager.isFragmentStackNull(screenId)) {
            Logger.i(TAG, "current fragment stack is null");
            toFragment.setArguments(bundle);
            transaction.add(containerId, toFragment);
            transaction.show(toFragment);
            mStackManager.push(screenId, toFragment);
            transaction.commitAllowingStateLoss();
            Logger.i(TAG, "current stack Fragment", mStackManager.getBaseFragmentStack(screenId).toString());
            Logger.i(TAG, "current List FragmentManager", fragmentManager.getFragments().toString());
            return;
        }
        BaseFragment currentFragment = mStackManager.getCurrentFragment(screenId);
        if (ConvertUtils.equals(currentFragment, toFragment)) {
            Logger.i(TAG, "current fragment == toFragment");
            toFragment.onNewIntent(bundle);
            Logger.i(TAG, "current stack Fragment", mStackManager.getBaseFragmentStack(screenId).toString());
            Logger.i(TAG, "current List FragmentManager", fragmentManager.getFragments().toString());
            return;
        }
        if (mStackManager.isContain(screenId, toFragment)) {
            Logger.i(TAG, "fragment stack 包含");
            mStackManager.removeBaseView(screenId, toFragment);
            mStackManager.push(screenId, toFragment);
            transaction.hide(currentFragment);
            transaction.show(toFragment);
            toFragment.onNewIntent(bundle);
        } else {
            Logger.i(TAG, "fragment stack 不包含");
            toFragment.setArguments(bundle);
            transaction.hide(currentFragment);
            transaction.add(containerId, toFragment);
            transaction.show(toFragment);
            mStackManager.push(screenId, toFragment);
        }
        transaction.commitAllowingStateLoss();
        Logger.i(TAG, "current stack Fragment", mStackManager.getBaseFragmentStack(screenId).toString());
        Logger.i(TAG, "current List FragmentManager", fragmentManager.getFragments().toString());
    }

    public static void closeFragment(String screenId, FragmentManager fragmentManager, BaseFragment fragment, boolean nextShow) {
        FragmentTransaction transaction = fragmentManager.beginTransaction();
        transaction.remove(fragment);
        mStackManager.removeBaseView(screenId, fragment);
        if (nextShow) {
            Logger.i(TAG, "移除上一个。显示下一个");
            BaseFragment toFragment = mStackManager.getCurrentFragment(screenId);
            if (!ConvertUtils.isEmpty(toFragment)) transaction.show(toFragment);
        }
        transaction.commitAllowingStateLoss();
        Logger.i(TAG, "current stack Fragment", mStackManager.getBaseFragmentStack(screenId).toString());
        Logger.i(TAG, "current List FragmentManager", fragmentManager.getFragments().toString());
    }

    public static void closeAllFragmentUpRoute(String screenId, FragmentManager fragmentManager) {
        FragmentTransaction transaction = fragmentManager.beginTransaction();
        List<Fragment> fragments = fragmentManager.getFragments();
        int index = -1;
        if (fragments.size() > 0) {
            for (int t = 0; t < fragments.size(); t++) {
                if (fragments.get(t).getClass().getName().contains("RouteFragment") || fragments.get(t).getClass().getName().contains("NaviGuidanceFragment")) {
                    index = t;
                    Logger.i(TAG, "remove top index: " + t);
                }
                if (index < t && index != -1) {
                    Logger.i(TAG, "transaction remove top fragment: " + fragments.get(t).getClass().getName());
                    transaction.remove(fragments.get(t));
                    if (!fragments.get(t).getClass().getName().contains("SupportRequestManagerFragment")) {
                        Logger.i(TAG, "mStackManager remove top fragment: " + fragments.get(t).getClass().getName());
                        mStackManager.removeBaseView(screenId, (IBaseView) fragments.get(t));
                    }
                }
            }
        }
        if (index != -1) transaction.show(fragments.get(index));
        transaction.commitAllowingStateLoss();
    }

    /**
     * 关闭包括targetFragment在内的所有上级fragment
     *
     * @param className 目标fragment的类名
     */
    public static void closeAllFragmentsUntilTargetFragment(String screenId, FragmentManager fragmentManager, String className) {
        FragmentTransaction transaction = fragmentManager.beginTransaction();
        List<Fragment> fragments = fragmentManager.getFragments();
        int index = -1;
        if (fragments.size() > 0) {
            for (int t = 0; t < fragments.size(); t++) {
                if (fragments.get(t).getClass().getName().contains(className)) {
                    index = t;
                    Logger.i(TAG, "remove top index: " + t);
                }
                if (index <= t && index != -1) {
                    Logger.i(TAG, "remove top fragment: " + fragments.get(t).getClass().getName());
                    transaction.remove(fragments.get(t));
                    mStackManager.removeBaseView(screenId, (IBaseView) fragments.get(t));
                }
            }
        }
        transaction.commitAllowingStateLoss();
    }

    public void closeFragment(boolean nextShow) {

    }

    public static void closeAllFragment(String screenId, FragmentManager fragmentManager) {
        FragmentTransaction transaction = fragmentManager.beginTransaction();
        if (fragmentManager.getFragments().size() <= 0) return;
        for (Fragment fragment : fragmentManager.getFragments()) {
            transaction.remove(fragment);
            if (fragment instanceof IBaseView) {
                mStackManager.removeBaseView(screenId, (IBaseView) fragment);
            } else {
                Logger.e(TAG, "cannot be cast IBaseView");
            }
        }
        transaction.commitAllowingStateLoss();
    }

    public void closeAllFragment() {
        mStackManager.removeAllFragment();
    }
}
