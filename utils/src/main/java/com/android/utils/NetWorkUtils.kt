package com.android.utils

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.net.ConnectivityManager
import android.net.LinkProperties
import android.net.Network
import android.net.NetworkCapabilities
import android.os.Build
import androidx.annotation.RequiresApi
import com.android.utils.log.Logger

/**
 * @Introduce: 网络状态监听.
 * @Author: lvww
 * @Date: 2023/11/13
 * @Description :可以通过{@link NetworkObserver}来监听设备的网络状态
 */
class NetWorkUtils private constructor() {
    private val tag: String = NetWorkUtils.javaClass.simpleName
    private var mContext: Context? = null
    private var connectivityManager: ConnectivityManager? = null
    private var networkReceiver: NetworkReceiver? = null
    private var networkCallback: NetworkCallback? = null
    private var netObservers: MutableList<NetworkObserver>? = null

    init {
        netObservers = ArrayList()
        networkCallback = NetworkCallback()
    }

    fun init(context: Context) {
        mContext = context
        connectivityManager =
            mContext?.getSystemService(Context.CONNECTIVITY_SERVICE) as ConnectivityManager?
//        mContext.registerReceiver(networkReceiver);
        connectivityManager?.registerDefaultNetworkCallback(networkCallback!!)
    }

    fun registerNetworkObserver(networkCall: NetworkObserver){
        netObservers?.add(networkCall)
    }

    fun unRegisterNetworkObserver(networkCall: NetworkObserver){
        netObservers?.remove(networkCall)
    }

    fun checkNetwork(): Boolean? {
        var networkCapabilities =
            connectivityManager?.getNetworkCapabilities(connectivityManager!!.activeNetwork)
        if (ConvertUtils.isNull(networkCapabilities)) return false
        return networkCapabilities?.hasCapability(NetworkCapabilities.NET_CAPABILITY_VALIDATED)
    }

    @RequiresApi(Build.VERSION_CODES.O)
    fun getNetworkTypeValue(): Int {
        if (checkNetwork() == false) return -1
        val networkCapabilities =
            connectivityManager?.getNetworkCapabilities(connectivityManager!!.activeNetwork)
        return if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_WIFI) == true) {
            if(Logger.openLog) {
                Logger.d(tag, "current network type is WIFI")
            }
            NetworkCapabilities.TRANSPORT_WIFI
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR) == true) {
            if(Logger.openLog) {
                Logger.d(tag, "current network type is 数据流量")
            }
            NetworkCapabilities.TRANSPORT_CELLULAR
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_ETHERNET) == true) {
            if(Logger.openLog) {
                Logger.d(tag, "current network type is 以太网")
            }
            NetworkCapabilities.TRANSPORT_CELLULAR
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_BLUETOOTH) == true) {
            NetworkCapabilities.TRANSPORT_BLUETOOTH
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_VPN) == true) {
            NetworkCapabilities.TRANSPORT_VPN
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_WIFI_AWARE) == true) {
            NetworkCapabilities.TRANSPORT_WIFI_AWARE
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_LOWPAN) == true) {
            NetworkCapabilities.TRANSPORT_LOWPAN
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_USB) == true) {
            NetworkCapabilities.TRANSPORT_USB
        } else {
            if(Logger.openLog) {
                Logger.d(tag, "current network type is 其他网络")
            }
            7
        }
    }

    fun getNetworkTypeStr(): String {
        if (checkNetwork() == false) return "无网络"
        var networkCapabilities =
            connectivityManager?.getNetworkCapabilities(connectivityManager!!.activeNetwork)
        return if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_WIFI) == true) {
            "current network type is 此网络使用WiFi传输"
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR) == true) {
            "current network type is 此网络使用蜂窝传输"
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_ETHERNET) == true) {
            "current network type is 此网络使用以太网传输"
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_BLUETOOTH) == true) {
            "current network type is 此网络使用蓝牙传输"
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_VPN) == true) {
            "current network type is 此网络使用VPN传输"
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_WIFI_AWARE) == true) {
            "current network type is 此网络使用Wi-Fi感知传输"
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_LOWPAN) == true) {
            "current network type is 此网络使用LoWpAn传输"
        } else if (networkCapabilities?.hasTransport(NetworkCapabilities.TRANSPORT_USB) == true) {
            "current network type is 此网络使用USB传输"
        } else {
            "current network type is 其他网络"
        }
    }

    fun clearCache() {
        mContext = null;
        connectivityManager?.unregisterNetworkCallback(networkCallback!!)
        netObservers?.clear()
        netObservers = null
    }

    inner class NetworkReceiver : BroadcastReceiver() {
        private val netACTION = "android.net.conn.CONNECTIVITY_CHANGE"
        override fun onReceive(context: Context?, intent: Intent?) {
            if (!ConvertUtils.equals(intent?.action, netACTION)) return
            val isBreak = intent!!.getBooleanExtra(ConnectivityManager.EXTRA_NO_CONNECTIVITY, false)
            if (isBreak) Logger.i(tag, "网络已断开")
            else Logger.i(tag, "网络已链接")
        }
    }

    inner class NetworkCallback : ConnectivityManager.NetworkCallback() {

        override fun onCapabilitiesChanged(network: Network, networkCapabilities: NetworkCapabilities) {
            super.onCapabilitiesChanged(network, networkCapabilities)
            if (networkCapabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_VALIDATED)) {
                Logger.i(tag, "网络验证完成")
                for (code in netObservers!!) {
                    code.onNetValidated() // 新增验证完成回调
                }
            }
        }

        override fun onAvailable(network: Network) {
            super.onAvailable(network)
            if(Logger.openLog) {
                Logger.i(tag, "网络连接成功,可以使用的时候调用")
            }
            for (code in netObservers!!){
                code.onNetConnectSuccess()
            }
        }

        override fun onUnavailable() {
            super.onUnavailable()
            if(Logger.openLog) {
                Logger.i(tag, "当网络连接超时或网络请求达不到可用要求时调用")
            }
        }

        override fun onBlockedStatusChanged(network: Network, blocked: Boolean) {
            super.onBlockedStatusChanged(network, blocked)
            if(Logger.openLog) {
                Logger.i(tag, "当访问指定的网络被阻止或解除阻塞时调用")
            }
        }

        override fun onLosing(network: Network, maxMsToLive: Int) {
            super.onLosing(network, maxMsToLive)
            if(Logger.openLog) {
                Logger.i(tag, "当网络正在断开连接时调用")
            }
        }

        override fun onLost(network: Network) {
            super.onLost(network)
            if(Logger.openLog) {
                Logger.i(tag, "当网络已断开连接时调用")
            }
            for (code in netObservers!!){
                code.onNetDisConnect()
            }
        }

        override fun onLinkPropertiesChanged(network: Network, linkProperties: LinkProperties) {
            super.onLinkPropertiesChanged(network, linkProperties)
            if(Logger.openLog) {
                Logger.i(tag, "当网络连接的属性被修改时调用")
            }
        }
    }

    interface NetworkObserver{
        fun onNetValidated()

        fun onNetConnectSuccess()

        fun onNetUnavailable()

        fun onNetBlockedStatusChanged()

        fun onNetLosing()

        fun onNetLinkPropertiesChanged()

        fun onNetDisConnect()
    }

    companion object {
        fun getInstance() = Helper.network
    }

    object Helper {
        val network = NetWorkUtils()
    }
}