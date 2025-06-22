package com.sgm.navi.service.greendao;

import androidx.annotation.WorkerThread;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.AppCache;

import org.greenrobot.greendao.database.Database;

import java.util.Date;
import java.util.List;


public final class CommonManager {
    private static final String TAG = CommonManager.class.getSimpleName();
    private static final String DB_NAME = "common.db";
    private static CommonManager mCommonManager;
    private CommonSettingDao mCommonSettingDao;
    private boolean mIsInit = false;
    private CommonManager() {

    }
    /**
     * Get instance.
     * @return SettingManager
     */
    public static CommonManager getInstance() {
        if (mCommonManager == null) {
            synchronized (CommonManager.class) {
                if (mCommonManager == null) {
                    mCommonManager = new CommonManager();
                }
                return mCommonManager;
            }
        }
        return mCommonManager;
    }

    /**
     * Init database and cloud data.
     */
    public void init() {
        if (mIsInit) {
            Logger.d(TAG, "Database had Initialized!");
            return;
        }
        // 数据库对象
        final DaoMaster.DevOpenHelper helper = new DaoMaster.DevOpenHelper(AppCache.getInstance().getMContext(), DB_NAME);
        final Database db = helper.getWritableDb();
        final DaoMaster daoMaster = new DaoMaster(db);
        mCommonSettingDao = daoMaster.newSession().getCommonSettingDao();
    }

    /**
     * Use key and value for data settings.
     * @param key save key
     * @param value save value
     */
    public void insertValue(final String key, final String value) {
        Logger.d(TAG, "key = " + key + "  value = " + value);
        if (searchFromDb(key)) {
            Logger.d(TAG, "update");
            update(key, value);
        } else {
            Logger.d(TAG, "insertOrReplace");
            insertOrReplace(key, value);
        }
    }

    /**
     * 查询数据库中是否有该key的数据
     * @param key search key
     * @return true if exist,false if not exist
     */
    private boolean searchFromDb(final String key) {
        final List<CommonSetting> saveData = mCommonSettingDao.queryBuilder()
                .where(CommonSettingDao.Properties.MName.eq(key))
                .list();
        return !saveData.isEmpty();
    }

    /**
     * 修改或者更新设置项数据
     * @param key 对应key值
     * @param value 对应值
     */
    @WorkerThread
    public void insertOrReplace(final String key, final String value) {
        ThreadManager.getInstance().asyncDelay(() -> {
            final CommonSetting setting = new CommonSetting();
            setting.setMName(key);
            setting.setMValue(value);
            setting.setMUpdateTime(new Date());
            mCommonSettingDao.insertOrReplace(setting);
        },0);
    }

    /**
     * 插入用户信息
     * @param key 对应key值
     * @param value 对应值
     */
    public void insertUserInfo(final String key, final String value) {
        final CommonSetting setting = new CommonSetting();
        setting.setMName(key);
        setting.setMValue(value);
        setting.setMUpdateTime(new Date());
        mCommonSettingDao.insertOrReplace(setting);
    }

    /**
     * 通过key查找其对应value
     * @param key 唯一值
     * @return value
     */
    public String getValueByKey(final String key) {
        final CommonSetting commonSetting = mCommonSettingDao.queryBuilder()
                .where(CommonSettingDao.Properties.MName.eq(key))
                .unique();
        if (commonSetting != null) {
            return commonSetting.getMValue();
        } else {
            return "";
        }
    }

    /**
     * 更新数据
     * @param key 对应key值
     * @param value 对应值
     */
    private void update(final String key, final String value) {
        final CommonSetting unique = mCommonSettingDao.queryBuilder()
                .where(CommonSettingDao.Properties.MName.eq(key))
                .unique();
        unique.setMValue(value);
        mCommonSettingDao.update(unique);
    }

    /**
     * 删除某个数据
     * @param key 对应key值
     */
    public void deleteValue(final String key) {
        mCommonSettingDao.queryBuilder()
                .where(CommonSettingDao.Properties.MName.eq(key))
                .buildDelete()
                .executeDeleteWithoutDetachingEntities();
    }

    /**
     * 清空全部数据
     */
    public void deleteAll() {
        mCommonSettingDao.deleteAll();
    }

}
