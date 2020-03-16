// Copyright Neal Gafter 2019.

//This program is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.

//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with this program.  If not, see <https://www.gnu.org/licenses/>.

using System.Collections.Generic;
using System.Threading;
using System.Runtime.CompilerServices;

namespace System.Runtime.CompilerServices
{
    public class TypeSwitchLockHolder

    {
        public static object _lock = new object();

    }
    public class MagicTuple<A, B> { }
    class ClassWithIntPtr
    {
        public IntPtr val;
    }

    public class TypeSwitchClassWrapper
    {
        private static Entry[] _buckets;
        private struct Entry
        {
            // We use a weak reference so that dispatching on a type does not prevent it from being unloaded.
            public IntPtr ReferenceToType;
            public int Result;

            // Use NoInlining to prevent optimization of the read
            [MethodImpl(MethodImplOptions.NoInlining)]
            public int GetResult() { return Result; }
        }
        public class TypeSwitchCache<T>
        {
            // These are allocated lazily.  If GetIndex is never called, _lock and _buckets are never allocated.
            private static Type[] _types;
            private static int _load;
            private const int ResultOffset = -2;

            private static void AddTypesFromType(Type t, List<Type> types)
            {
                if (t.IsGenericType && t.GetGenericTypeDefinition() == typeof(MagicTuple<,>))
                {
                    foreach (Type nestedType in t.GetGenericArguments())
                    {
                        AddTypesFromType(nestedType, types);
                    }
                }
                else
                    types.Add(t);
            }

            private static Type[] ComputeTypesList()
            {
                List<Type> types = new List<Type>();
                AddTypesFromType(typeof(TypeSwitchCache<T>).GenericTypeArguments[0], types);
                return types.ToArray();
            }

            [MethodImpl(MethodImplOptions.NoInlining)]
            private static Entry[] GetBuckets()
            {
                return _buckets;
            }

            public static int TypeSwitch(object obj)
            {
                if (obj == null)
                    return -1;
                ClassWithIntPtr c = Unsafe.As<ClassWithIntPtr>(obj);
                IntPtr typeValue = Unsafe.Add(ref c.val, -1);
                int typeHash = (int)(((long)typeValue) >> 3);

                Entry[] buckets = Volatile.Read(ref _buckets);
                if (buckets != null)
                {
                    int nBuckets = buckets.Length;
                    int mask = nBuckets - 1;

                    int startBucket = typeHash & mask;
                    for (int i = 0; i < nBuckets; i++)
                    {
                        int bucket = (i + startBucket) & mask;
                        ref Entry entry = ref buckets[bucket];
                        var entryReferenceToType = entry.ReferenceToType;
                        if (entryReferenceToType == IntPtr.Zero)
                        {
                            // not found; insert it!
                            break;
                        }

                        if (entryReferenceToType == typeValue)
                        {
                            int result = Volatile.Read(ref entry.Result);
                            if (result != 0)
                                return result + ResultOffset;
                            else
                                break;
                        }
                    }
                }
                return GetIndexSlow(obj);
            }

            private static int SearchInBuckets(Entry[] buckets, object obj, out int chainLength, out bool addedItem)
            {
                addedItem = false;
                chainLength = 0;

                ClassWithIntPtr c = Unsafe.As<ClassWithIntPtr>(obj);
                IntPtr typeValue = Unsafe.Add(ref c.val, -1);
                int typeHash = (int)(((long)typeValue) >> 3);

                int nBuckets = buckets.Length;
                int mask = nBuckets - 1;

                int startBucket = typeHash & mask;
                for (int i = 0; i < nBuckets; i++)
                {
                    int bucket = (i + startBucket) & mask;
                    ref Entry entry = ref buckets[bucket];
                    var entryReferenceToType = entry.ReferenceToType;
                    if (entryReferenceToType == IntPtr.Zero)
                    {
                        addedItem = true;
                        //                    Type objType = obj.GetType();
                        int result = ComputeResult(obj.GetType());
                        entry.Result = result - ResultOffset;
                        entry.ReferenceToType = typeValue;
                        chainLength = i;
                        return result;
                    }

                    if (entryReferenceToType == typeValue)
                    {
                        int result = entry.GetResult();
                        if (result != 0)
                            return result + ResultOffset;
                        else
                            throw new Exception("Cannot Happen, ReferenceToType was non-null, but result was 0");
                    }
                }
                // Unreachable
                return -2;

            }

            /// <summary>
            /// Compute the result.
            /// </summary>
            private static int ComputeResult(Type type)
            {
                Type[] types = _types;
                for (int i = 0, n = types.Length; i < n; i++)
                {
                    if (types[i].IsAssignableFrom(type))
                        return i;
                }

                return types.Length;
            }

            private static int GetIndexSlow(object obj)
            {
                lock (TypeSwitchLockHolder._lock)
                {
                    if (_buckets == null)
                    {
                        _buckets = new Entry[8];
                        _types = ComputeTypesList();
                    }

                    Entry[] oldBuckets = _buckets;

                    int result = SearchInBuckets(oldBuckets, obj, out int chainLength, out bool addedItem);
                    if (addedItem)
                    {
                        // Grow table if necessary
                        if ((++_load) > (oldBuckets.Length / 4) || (chainLength > 2))
                        {
                            // This is a cache, so its free to throw out old data
                            _buckets = new Entry[oldBuckets.Length * 2];
                            _load = 0;
                        }
                    }
                    return result;
                }
            }
        }
    }

    public class TypeSwitchClassWrapperNoNullChecking
    {
        private static Entry[] _buckets;
        private struct Entry
        {
            // We use a weak reference so that dispatching on a type does not prevent it from being unloaded.
            public IntPtr ReferenceToType;
            public int Result;

            // Use NoInlining to prevent optimization of the read
            [MethodImpl(MethodImplOptions.NoInlining)]
            public int GetResult() { return Result; }
        }
        public class TypeSwitchCache<T>
        {
            // These are allocated lazily.  If GetIndex is never called, _lock and _buckets are never allocated.
            private static Type[] _types;
            private static int _load;
            private const int ResultOffset = -2;

            private static void AddTypesFromType(Type t, List<Type> types)
            {
                if (t.IsGenericType && t.GetGenericTypeDefinition() == typeof(MagicTuple<,>))
                {
                    foreach (Type nestedType in t.GetGenericArguments())
                    {
                        AddTypesFromType(nestedType, types);
                    }
                }
                else
                    types.Add(t);
            }

            private static Type[] ComputeTypesList()
            {
                List<Type> types = new List<Type>();
                AddTypesFromType(typeof(TypeSwitchCache<T>).GenericTypeArguments[0], types);
                return types.ToArray();
            }

            [MethodImpl(MethodImplOptions.NoInlining)]
            private static Entry[] GetBuckets()
            {
                return _buckets;
            }

            public static int TypeSwitch(object obj)
            {
                ClassWithIntPtr c = Unsafe.As<ClassWithIntPtr>(obj);
                IntPtr typeValue = Unsafe.Add(ref c.val, -1);
                int typeHash = (int)(((long)typeValue) >> 3);

                Entry[] buckets = Volatile.Read(ref _buckets);
                if (buckets != null)
                {
                    int nBuckets = buckets.Length;
                    int mask = nBuckets - 1;

                    int startBucket = typeHash & mask;
                    for (int i = 0; i < nBuckets; i++)
                    {
                        int bucket = (i + startBucket) & mask;
                        ref Entry entry = ref buckets[bucket];
                        var entryReferenceToType = entry.ReferenceToType;
                        if (entryReferenceToType == IntPtr.Zero)
                        {
                            // not found; insert it!
                            break;
                        }

                        if (entryReferenceToType == typeValue)
                        {
                            int result = Volatile.Read(ref entry.Result);
                            if (result != 0)
                                return result + ResultOffset;
                            else
                                break;
                        }
                    }
                }
                return GetIndexSlow(obj);
            }

            private static int SearchInBuckets(Entry[] buckets, object obj, out int chainLength, out bool addedItem)
            {
                addedItem = false;
                chainLength = 0;

                ClassWithIntPtr c = Unsafe.As<ClassWithIntPtr>(obj);
                IntPtr typeValue = Unsafe.Add(ref c.val, -1);
                int typeHash = (int)(((long)typeValue) >> 3);

                int nBuckets = buckets.Length;
                int mask = nBuckets - 1;

                int startBucket = typeHash & mask;
                for (int i = 0; i < nBuckets; i++)
                {
                    int bucket = (i + startBucket) & mask;
                    ref Entry entry = ref buckets[bucket];
                    var entryReferenceToType = entry.ReferenceToType;
                    if (entryReferenceToType == IntPtr.Zero)
                    {
                        addedItem = true;
                        //                    Type objType = obj.GetType();
                        int result = ComputeResult(obj.GetType());
                        entry.Result = result - ResultOffset;
                        entry.ReferenceToType = typeValue;
                        chainLength = i;
                        return result;
                    }

                    if (entryReferenceToType == typeValue)
                    {
                        int result = entry.GetResult();
                        if (result != 0)
                            return result + ResultOffset;
                        else
                            throw new Exception("Cannot Happen, ReferenceToType was non-null, but result was 0");
                    }
                }
                // Unreachable
                return -2;

            }

            /// <summary>
            /// Compute the result.
            /// </summary>
            private static int ComputeResult(Type type)
            {
                Type[] types = _types;
                for (int i = 0, n = types.Length; i < n; i++)
                {
                    if (types[i].IsAssignableFrom(type))
                        return i;
                }

                return types.Length;
            }

            private static int GetIndexSlow(object obj)
            {
                lock (TypeSwitchLockHolder._lock)
                {
                    if (_buckets == null)
                    {
                        _buckets = new Entry[8];
                        _types = ComputeTypesList();
                    }

                    Entry[] oldBuckets = _buckets;

                    int result = SearchInBuckets(oldBuckets, obj, out int chainLength, out bool addedItem);
                    if (addedItem)
                    {
                        // Grow table if necessary
                        if ((++_load) > (oldBuckets.Length / 4) || (chainLength > 2))
                        {
                            // This is a cache, so its free to throw out old data
                            _buckets = new Entry[oldBuckets.Length * 2];
                            _load = 0;
                        }
                    }
                    return result;
                }
            }
        }
    }

    public class TypeSwitchClassWrapperNoNullChecking_PrimeNumberHashtableSize
    {
        private static Entry[] _buckets;
        private struct Entry
        {
            // We use a weak reference so that dispatching on a type does not prevent it from being unloaded.
            public IntPtr ReferenceToType;
            public int Result;

            // Use NoInlining to prevent optimization of the read
            [MethodImpl(MethodImplOptions.NoInlining)]
            public int GetResult() { return Result; }
        }
        public class TypeSwitchCache<T>
        {
            // These are allocated lazily.  If GetIndex is never called, _lock and _buckets are never allocated.
            private static Type[] _types;
            private static int _load;
            private const int ResultOffset = -2;
            private const int ExcessLinearProbeSpace = 4;

            private static void AddTypesFromType(Type t, List<Type> types)
            {
                if (t.IsGenericType && t.GetGenericTypeDefinition() == typeof(MagicTuple<,>))
                {
                    foreach (Type nestedType in t.GetGenericArguments())
                    {
                        AddTypesFromType(nestedType, types);
                    }
                }
                else
                    types.Add(t);
            }

            private static Type[] ComputeTypesList()
            {
                List<Type> types = new List<Type>();
                AddTypesFromType(typeof(TypeSwitchCache<T>).GenericTypeArguments[0], types);
                return types.ToArray();
            }

            [MethodImpl(MethodImplOptions.NoInlining)]
            private static Entry[] GetBuckets()
            {
                return _buckets;
            }

            public static int TypeSwitch(object obj)
            {
                ClassWithIntPtr c = Unsafe.As<ClassWithIntPtr>(obj);
                IntPtr typeValue = Unsafe.Add(ref c.val, -1);
                uint typeHash = (uint)(((ulong)typeValue) >> 3);

                Entry[] buckets = Volatile.Read(ref _buckets);
                if (buckets != null)
                {
                    uint nBuckets = (uint)buckets.Length - ExcessLinearProbeSpace;

                    uint bucket = typeHash % nBuckets;
                    for (; true; bucket++)
                    {
                        ref Entry entry = ref buckets[bucket];
                        var entryReferenceToType = entry.ReferenceToType;
                        if (entryReferenceToType == IntPtr.Zero)
                        {
                            // not found; insert it!
                            break;
                        }

                        if (entryReferenceToType == typeValue)
                        {
                            int result = Volatile.Read(ref entry.Result);
                            if (result > 1)
                                return result + ResultOffset;
                            else
                                return GetIndexSlow(result, obj);
                        }
                    }
                }
                return GetIndexSlow(-2, obj);
            }

            private static int SearchInBuckets(Entry[] buckets, object obj, out int chainLength, out bool addedItem)
            {
                addedItem = false;
                chainLength = 0;

                ClassWithIntPtr c = Unsafe.As<ClassWithIntPtr>(obj);
                IntPtr typeValue = Unsafe.Add(ref c.val, -1);
                uint typeHash = (uint)(((ulong)(long)typeValue) >> 3);

                uint nBuckets = (uint)buckets.Length - ExcessLinearProbeSpace;

                uint startBucket = typeHash % nBuckets;
                for (uint i = 0; true; i++)
                {
                    uint bucket = i + startBucket;

                    ref Entry entry = ref buckets[bucket];
                    var entryReferenceToType = entry.ReferenceToType;
                    if (entryReferenceToType == IntPtr.Zero)
                    {
                        addedItem = true;
                        //                    Type objType = obj.GetType();
                        int result = ComputeResult(obj.GetType());
                        entry.Result = result - ResultOffset;
                        entry.ReferenceToType = typeValue;
                        chainLength = (int)i;
                        return result;
                    }

                    if (entryReferenceToType == typeValue)
                    {
                        int result = entry.GetResult();
                        if (result != 0)
                            return result + ResultOffset;
                        else
                            throw new Exception("Cannot Happen, ReferenceToType was non-null, but result was 0");
                    }
                }
            }

            /// <summary>
            /// Compute the result.
            /// </summary>
            private static int ComputeResult(Type type)
            {
                Type[] types = _types;
                for (int i = 0, n = types.Length; i < n; i++)
                {
                    if (types[i].IsAssignableFrom(type))
                        return i;
                }

                return types.Length;
            }

            private static int GetIndexSlow(int result, object obj)
            {
                if (result == -1)
                {
                    // TODO handle COM and ICastable scenarios
                }
                lock (TypeSwitchLockHolder._lock)
                {
                    if (_buckets == null)
                    {
                        _buckets = new Entry[7 + ExcessLinearProbeSpace];
                        _types = ComputeTypesList();
                    }

                    Entry[] oldBuckets = _buckets;

                    result = SearchInBuckets(oldBuckets, obj, out int chainLength, out bool addedItem);
                    if (addedItem)
                    {
                        // Grow table if necessary
                        if (chainLength > 2)
                        {
                            // This is a cache, so its free to throw out old data
                            _buckets = new Entry[System.Collections.HashHelpers.GetPrime(oldBuckets.Length - ExcessLinearProbeSpace + 1) + ExcessLinearProbeSpace];
                            _load = 0;
                        }
                    }
                    return result;
                }
            }
        }
    }

    public class TypeSwitchCache<T>
    {
        private static Entry[] _buckets;
        private struct Entry
        {
            // We use a weak reference so that dispatching on a type does not prevent it from being unloaded.
            public IntPtr ReferenceToType;
            public int Result;

            // Use NoInlining to prevent optimization of the read
            [MethodImpl(MethodImplOptions.NoInlining)]
            public int GetResult() { return Result; }
        }

        // These are allocated lazily.  If GetIndex is never called, _lock and _buckets are never allocated.
        private static Type[] _types;
        private static int _load;
        private const int ResultOffset = -2;

        private static void AddTypesFromType(Type t, List<Type> types)
        {
            if (t.IsGenericType && t.GetGenericTypeDefinition() == typeof(MagicTuple<,>))
            {
                foreach (Type nestedType in t.GetGenericArguments())
                {
                    AddTypesFromType(nestedType, types);
                }
            }
            else
                types.Add(t);
        }

        private static Type[] ComputeTypesList()
        {
            List<Type> types = new List<Type>();
            AddTypesFromType(typeof(TypeSwitchCache<T>).GenericTypeArguments[0], types);
            return types.ToArray();
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static Entry[] GetBuckets()
        {
            return _buckets;
        }

        public static int TypeSwitch(object obj)
        {
            if (obj == null)
                return -1;
            ClassWithIntPtr c = Unsafe.As<ClassWithIntPtr>(obj);
            IntPtr typeValue = Unsafe.Add(ref c.val, -1);
            int typeHash = (int)(((long)typeValue) >> 3);

            Entry[] buckets = Volatile.Read(ref _buckets);
            if (buckets != null)
            {
                int nBuckets = buckets.Length;
                int mask = nBuckets - 1;

                int startBucket = typeHash & mask;
                for (int i = 0; i < nBuckets; i++)
                {
                    int bucket = (i + startBucket) & mask;
                    ref Entry entry = ref buckets[bucket];
                    var entryReferenceToType = entry.ReferenceToType;
                    if (entryReferenceToType == IntPtr.Zero)
                    {
                        // not found; insert it!
                        break;
                    }

                    if (entryReferenceToType == typeValue)
                    {
                        int result = Volatile.Read(ref entry.Result);
                        if (result != 0)
                            return result + ResultOffset;
                        else
                            break;
                    }
                }
            }
            return GetIndexSlow(obj);
        }

        private static int SearchInBuckets(Entry[] buckets, object obj, out int chainLength, out bool addedItem)
        {
            addedItem = false;
            chainLength = 0;

            ClassWithIntPtr c = Unsafe.As<ClassWithIntPtr>(obj);
            IntPtr typeValue = Unsafe.Add(ref c.val, -1);
            int typeHash = (int)(((long)typeValue) >> 3);

            int nBuckets = buckets.Length;
            int mask = nBuckets - 1;

            int startBucket = typeHash & mask;
            for (int i = 0; i < nBuckets; i++)
            {
                int bucket = (i + startBucket) & mask;
                ref Entry entry = ref buckets[bucket];
                var entryReferenceToType = entry.ReferenceToType;
                if (entryReferenceToType == IntPtr.Zero)
                {
                    addedItem = true;
                    //                    Type objType = obj.GetType();
                    int result = ComputeResult(obj.GetType());
                    entry.Result = result - ResultOffset;
                    entry.ReferenceToType = typeValue;
                    chainLength = i;
                    return result;
                }

                if (entryReferenceToType == typeValue)
                {
                    int result = entry.GetResult();
                    if (result != 0)
                        return result + ResultOffset;
                    else
                        throw new Exception("Cannot Happen, ReferenceToType was non-null, but result was 0");
                }
            }
            // Unreachable
            return -2;

        }

        /// <summary>
        /// Compute the result.
        /// </summary>
        private static int ComputeResult(Type type)
        {
            Type[] types = _types;
            for (int i = 0, n = types.Length; i < n; i++)
            {
                if (types[i].IsAssignableFrom(type))
                    return i;
            }

            return types.Length;
        }

        private static int GetIndexSlow(object obj)
        {
            lock (TypeSwitchLockHolder._lock)
            {
                if (_buckets == null)
                {
                    _buckets = new Entry[8];
                    _types = ComputeTypesList();
                }

                Entry[] oldBuckets = _buckets;

                int result = SearchInBuckets(oldBuckets, obj, out int chainLength, out bool addedItem);
                if (addedItem)
                {
                    // Grow table if necessary
                    if ((++_load) > (oldBuckets.Length / 4) || (chainLength > 2))
                    {
                        // This is a cache, so its free to throw out old data
                        _buckets = new Entry[oldBuckets.Length * 2];
                        _load = 0;
                    }
                }
                return result;
            }
        }
    }

    /// <summary>
    /// A type that computes the first type in a list of types that a given type has a reference conversion to.
    /// It can be used to efficiently implement a type switch for a large number of types.
    /// </summary>
    public struct TypeSwitchDispatch
    {
        // The elements of this array do not need to be weak as the Dispatch would typically be stored in a static
        // variable of a class that explicitly references these types.  Until the referencing code is unloaded,
        // those referenced types cannot be unloaded.  And once the referencing code is unloaded, the static
        // variable containing the instance of this type would be gone.
        private readonly Type[] _types;

        // These are allocated lazily.  If GetIndex is never called, _lock and _buckets are never allocated.
        private object _lock;
        private Entry[] _buckets;
        private int _nEntries;

        /// <summary>
        /// The initial number of buckets, which must be a power of two.  Since we only expect this to be
        /// used for more than 10 types, and we maintain a load factor in the cache of less than 1/4, we
        /// use the first power of two greater than 10*4.
        /// </summary>
        private const int _initialBuckets = 64;

        private struct Entry
        {
            // We use a weak reference so that dispatching on a type does not prevent it from being unloaded.
            public WeakReference<Type> ReferenceToType;
            public int TypeHash;
            public int Result;
        }

        public TypeSwitchDispatch(params Type[] types)
        {
            if (types is null)
                throw new ArgumentNullException("types");

            int n = types.Length;
            var copiedTypesArray = new Type[n];
            for (int i = 0; i < n; i++)
            {
                var type = types[i];
                if (type is null)
                    throw new ArgumentNullException($"types[{i}]");

                copiedTypesArray[i] = type;
            }

            this._types = copiedTypesArray;
            this._nEntries = 0;

            // _lock and _buckets are created lazily
            this._lock = null;
            this._buckets = null;
        }

        /// <summary>
        /// Get the index of the first type in the original list of types to which this data's type can be assigned.
        /// </summary>
        public int GetIndex(object data)
        {
            if (data is null)
                return -1;

            Type type = data.GetType();
            int typeHash = type.GetHashCode();

            // First, we try a cache fetch without locks.  If the entry is not found, we lock and do it the hard way.
            Entry[] buckets = this._buckets;
            if (buckets is null)
                return GetIndexSlow(type, typeHash);

            int nBuckets = buckets.Length;
            int mask = nBuckets - 1;

            int startBucket = typeHash & mask;
            for (int i = 0; i < nBuckets; i++)
            {
                int bucket = (i + startBucket) & mask;
                ref Entry entry = ref buckets[bucket];
                var entryReferenceToType = Volatile.Read(ref entry.ReferenceToType);
                if (entryReferenceToType is null)
                {
                    // not found; insert it!
                    return GetIndexSlow(type, typeHash);
                }

                var entryTypeHash = Volatile.Read(ref entry.TypeHash);
                if (entryTypeHash == typeHash && entryReferenceToType.TryGetTarget(out Type entryType) && type.Equals(entryType))
                {
                    return Volatile.Read(ref entry.Result);
                }
            }

            throw new Exception("This location is believed unreachable.");
        }

        private int GetIndexSlow(Type type, int typeHash)
        {
            if (this._lock is null)
                Interlocked.CompareExchange(ref this._lock, new object(), null);

            lock (this._lock)
            {
                if (this._buckets is null)
                    this._buckets = new Entry[_initialBuckets];

                retry:;
                var buckets = this._buckets;
                int nBuckets = buckets.Length;
                int mask = nBuckets - 1;

                int startBucket = typeHash & mask;
                for (int i = 0; i < nBuckets; i++)
                {
                    int bucket = (i + startBucket) & mask;
                    ref Entry entry = ref buckets[bucket];
                    if (entry.ReferenceToType is null)
                    {
                        // not found; insert it!
                        if (ExpandIfNecessary())
                            goto retry;

                        int result = ComputeResult(type);
                        Volatile.Write(ref entry.Result, result);
                        Volatile.Write(ref entry.TypeHash, typeHash);
                        Volatile.Write(ref entry.ReferenceToType, new WeakReference<Type>(type));
                        this._nEntries++;
                        return result;
                    }
                    else if (entry.TypeHash == typeHash && entry.ReferenceToType.TryGetTarget(out Type entryType) && type.Equals(entryType))
                    {
                        return entry.Result;
                    }
                }

                throw new Exception("Unreachable");
            }
        }

        /// <summary>
        /// Compute the result.
        /// </summary>
        private int ComputeResult(Type type)
        {
            Type[] types = this._types;
            for (int i = 0, n = types.Length; i < n; i++)
            {
                if (types[i].IsAssignableFrom(type))
                    return i;
            }

            return -1;
        }

        /// <summary>
        /// Expand the <see cref="_buckets"/> array if necessary.  To be called while the lock is held.
        /// </summary>
        /// <returns>true if we expanded the <see cref="_buckets"/> array.</returns>
        private bool ExpandIfNecessary()
        {
            // Maintain a load factor of less than 1/4
            var buckets = this._buckets;
            int nBuckets = buckets.Length;
            if ((this._nEntries << 2) < nBuckets)
                return false;

            int newNBuckets = nBuckets << 1;
            int newMask = newNBuckets - 1;
            var newBuckets = new Entry[newNBuckets];

            for (int j = 0; j < nBuckets; j++)
            {
                var entryToMove = buckets[j];
                if (entryToMove.ReferenceToType is null)
                {
                    continue;
                }
                else if (!entryToMove.ReferenceToType.TryGetTarget(out _))
                {
                    // clear an expired weak reference while expanding
                    this._nEntries--;
                    continue;
                }

                int movedStartBucket = entryToMove.TypeHash & newMask;
                for (int k = 0; k < newNBuckets; k++)
                {
                    int newBucket = (k + movedStartBucket) & newMask;
                    ref Entry bucketEntry = ref newBuckets[newBucket];
                    if (bucketEntry.ReferenceToType is null)
                    {
                        bucketEntry = entryToMove;
                        goto nextEntry;
                    }
                }

                throw new Exception("This location is believed unreachable.");

            nextEntry:;
            }

            this._buckets = newBuckets;
            return true;
        }
    }
}
